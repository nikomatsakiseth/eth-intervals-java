package ch.ethz.intervals.visualizer;

import java.awt.BorderLayout;
import java.util.Arrays;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

@SuppressWarnings("serial")
public class InfoPanel 
extends JPanel
{
	
	JLabel intervalNameLabel;
	JTable connectionsTable;
	
	static final String[] cols = new String[] {
		"From", "To"
	};
	static final TableModel emptyModel = new DefaultTableModel(cols, 0);
	
	InfoPanel() {
		setLayout(new BorderLayout());
		
		intervalNameLabel = new JLabel();
		add(intervalNameLabel, BorderLayout.NORTH);
		
		connectionsTable = new JTable(emptyModel);
		add(connectionsTable, BorderLayout.CENTER);
	}
	
	public void loadInfoFor(IntervalInfo inter) {
		if(inter == null) {
			intervalNameLabel.setText("");
			connectionsTable.setModel(emptyModel);
		} else {
			intervalNameLabel.setText(inter.label);
			
			DefaultTableModel model = new DefaultTableModel(cols, 0);
			
			addEdges(model, inter.start.preds(), Arrays.asList(inter.start));
			addEdges(model, inter.end.preds(), Arrays.asList(inter.end));
			addEdges(model, Arrays.asList(inter.start), inter.start.succs());
			addEdges(model, Arrays.asList(inter.end), inter.end.succs());
			
			connectionsTable.setModel(model);
		}
	}

	private void addEdges(
			DefaultTableModel model, 
			Iterable<PointInfo> preds,
			Iterable<PointInfo> succs) 
	{
		for(PointInfo pred : preds)
			for(PointInfo succ : succs) {
				model.addRow(new String[] {
						pointString(pred), pointString(succ)
				});
			}
	}

	private String pointString(PointInfo pred) {
		return String.format("%d: %s(%s)", 
				pred.id, 
				(pred.isStart() ? "Start" : "End"),
				pred.inter.label);
	}
	
}
