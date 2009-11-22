package ch.ethz.intervals.visualizer;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;

import ch.ethz.intervals.visualizer.EventLog.Event;

@SuppressWarnings("serial")
public class VisualizerWindow extends JFrame {

    public VisualizerWindow() {

        setTitle("Intervals Visualizer");
        
        Toolkit toolkit = getToolkit();
        Dimension size = toolkit.getScreenSize();
        
        constructMenuBar();
        constructWindowContents(size);
        
        setSize(size.width*3/4, size.height*3/4);
        setLocation(size.width/2 - getWidth()/2, size.height/2 - getHeight()/2);
        setLocationRelativeTo(null);
        setVisible(true);
        
        constructGraphPanel();
    }
    
    final List<Event> events = new ArrayList<Event>();
    final DefaultListModel eventListModel = new DefaultListModel();
    final UserEventLogVisitor populateEventListModel = new UserEventLogVisitor(eventListModel);
    
    boolean followNewEvents = true;
    JList eventList;
    GraphPanel graphPanel;
    InfoPanel infoPanel;
    GraphPanelController graphPanelController;
    JScrollPane scrollGraphPanel;
    
    FileFilter executionLogFilter = new FileFilter() {
		
		@Override
		public String getDescription() {
			return "Intervals Execution Log";
		}
		
		@Override
		public boolean accept(File f) {
			return f.getName().endsWith(".executionLog");
		}
		
	};

	private void constructWindowContents(Dimension screenSize) {
		Container contentPane = getContentPane();
		
		eventList = new JList(eventListModel);
		eventList.setAutoscrolls(true);
		eventList.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		eventList.getSelectionModel().addListSelectionListener(new EventSelectedListener());
		JScrollPane scrollEventList = new JScrollPane(eventList);
		
		JPanel emptyPanel = new JPanel();
		scrollGraphPanel = new JScrollPane(emptyPanel);
		scrollGraphPanel.setAutoscrolls(true);
		scrollGraphPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		scrollGraphPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		
		infoPanel = new InfoPanel();
		JSplitPane graphInfoSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, 
				scrollGraphPanel, infoPanel);
		graphInfoSplit.setResizeWeight(0.75);
		graphInfoSplit.setDividerLocation(0.75);
		
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
        		scrollEventList, graphInfoSplit);
        splitPane.setOneTouchExpandable(true);
        splitPane.setDividerLocation(300);
		
		contentPane.add(splitPane);
	}
	
	private void constructGraphPanel() {
    	Graphics gfx = getGraphics();
		GraphPanel.selectFont(gfx);
    	FontMetrics fm = gfx.getFontMetrics();
    	GraphLayoutMetrics glm = new GraphLayoutMetrics(20, 20, 20, 10, 30, 3, 0, fm);
    	
    	ConflictMap conflictMap = new ConflictMap(glm);
		graphPanel = new GraphPanel(conflictMap, glm, new DefaultLayerOuter(conflictMap, glm));
		graphPanelController = new GraphPanelController(scrollGraphPanel, graphPanel, infoPanel);
		scrollGraphPanel.setViewportView(graphPanel);		
       	scrollGraphPanel.revalidate();
       	scrollGraphPanel.repaint();
	}
	
	private void constructMenuBar() {
		JMenuBar menubar = new JMenuBar();
        ImageIcon icon = new ImageIcon("exit.png");

        JMenu file = new JMenu("File");
        file.setMnemonic(KeyEvent.VK_F);

        JMenuItem fileOpenMostRecent = new JMenuItem("Open most recent", icon);
        fileOpenMostRecent.setMnemonic(KeyEvent.VK_O);
        fileOpenMostRecent.setToolTipText("Open most recently generated log");
        fileOpenMostRecent.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
            	openMostRecent();
            }
        });
        file.add(fileOpenMostRecent);
        
        JMenuItem fileOpen = new JMenuItem("Open...", icon);
        fileOpen.setMnemonic(KeyEvent.VK_O);
        fileOpen.setToolTipText("Open a new log");
        fileOpen.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
            	String tmpDir = System.getProperty("java.io.tmpdir");
            	JFileChooser fc = new JFileChooser(tmpDir);
            	fc.setFileFilter(executionLogFilter);
            	int rv = fc.showOpenDialog(VisualizerWindow.this);
            	if(rv == JFileChooser.APPROVE_OPTION) {
            		File file = fc.getSelectedFile();
            		openExecutionLog(file);
            	}
            }
        });
        file.add(fileOpen);

        JMenuItem fileExit = new JMenuItem("Exit", icon);
        fileExit.setMnemonic(KeyEvent.VK_C);
        fileExit.setToolTipText("Exit application");
        fileExit.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
            	if(getDefaultCloseOperation() == EXIT_ON_CLOSE)
            		System.exit(0);
            	else
            		VisualizerWindow.this.dispose();
            }

        });
        file.add(fileExit);
        
        menubar.add(file);

        setJMenuBar(menubar);
	}
	
	protected void openMostRecent() {
       	File tmpDir = new File(System.getProperty("java.io.tmpdir"));
       	File mostRecent = null;
       	for(File file : tmpDir.listFiles()) {
       		if(executionLogFilter.accept(file)) {
       			if(mostRecent == null || mostRecent.lastModified() < file.lastModified())
       				mostRecent = file;
       		}
       	}
        	
       	if(mostRecent != null)
       		openExecutionLog(mostRecent);
       	else
       		JOptionPane.showMessageDialog(VisualizerWindow.this, "No executionLogs found in temporary directory");			
	}

    protected void openExecutionLog(final File file) {
    	events.clear();
    	eventListModel.clear();
		EventLogParser.startParse(this, file);
	}
    
	public void processNewEvent(Event chunk) {
		int idx = events.size();
		int oldListSize = eventListModel.size();
		events.add(chunk);
		chunk.accept(populateEventListModel, idx);	
		int newListSize = eventListModel.size();
		
		// if at last row, and a new user event was produced, advance
		if(newListSize != oldListSize && followNewEvents)
			eventList.setSelectedIndex(newListSize - 1);
	}
	
    /** Invoked by the table when the user clicks on a row. */
    class EventSelectedListener implements ListSelectionListener {
		@Override
		public void valueChanged(ListSelectionEvent e) {
			int ueIdx = eventList.getSelectedIndex();
            UserEvent ue = (UserEvent) eventListModel.get(ueIdx);
            int evIdx = ue.sourceIndex;            
//            System.err.printf("Selection changed to %d from %d\n", evIdx, selectedRow);
            
            // keep following events so long as the thing which was selected is
            // the last one in the list
            followNewEvents = (ueIdx == eventListModel.size() - 1);
            
            int currentTime = graphPanelController.currentTime;
            if(evIdx > currentTime) {
            	for(int i = currentTime + 1; i <= evIdx; i++)
            		events.get(i).accept(graphPanel.doVisitor, i);
            } else if (evIdx < currentTime) {
            	for(int i = currentTime; i > evIdx; i--)
            		events.get(i).accept(graphPanel.undoVisitor, i);
            }

            graphPanel.setSize(graphPanel.getPreferredSize());
            graphPanelController.currentTime = evIdx;
           	scrollGraphPanel.revalidate();
           	scrollGraphPanel.repaint();
           	
           	events.get(evIdx).accept(graphPanelController.selectionDoVisitor, evIdx);           	
		}    	
    }

	public static void main(String[] args) {

		VisualizerWindow vw = new VisualizerWindow();
        vw.setDefaultCloseOperation(EXIT_ON_CLOSE);
        vw.openMostRecent();
        
    }

}