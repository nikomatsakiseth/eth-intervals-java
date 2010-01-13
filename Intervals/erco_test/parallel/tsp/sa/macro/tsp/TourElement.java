package sa.macro.tsp;

public class TourElement {

    int[] prefix=new int[Tsp.MAX_TOUR_SIZE];
    int conn;
    int last;
    int prefix_weight;
    int lower_bound;
    int mst_weight;

}
