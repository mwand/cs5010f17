/**
 * Defines some examples for testing and makes them available as
 * public members of the class.
 */

public class FlightExamples {

    public static RacketList<Flight> panAmFlights = RacketLists.empty();

    public static RacketList<Flight> deltaFlights = initDeltaFlights();

    public static RacketList<Flight> deltaCycle = initDeltaCycle();

    // GIVEN: the name of a flight, the airports of departure and
    //     arrival, and the departure and arrival times represented
    //     as integers using the encoding of Problem Set 00,
    // RETURNS: a flight with the given name, airports, and times

    private static Flight flt (String s, String ap1, String ap2,
                               int t1, int t2) {
        UTC lv = UTCs.make (t1 / 100, t1 % 100);
        UTC ar = UTCs.make (t2 / 100, t2 % 100);
        return Flights.make (s, ap1, ap2, lv, ar);
    }

    // Returns a list of flights extracted from schedules published
    // by Delta Airlines.

    private static RacketList<Flight> initDeltaFlights() {
        RacketList<Flight> fs00 = RacketLists.empty();
        RacketList<Flight> fs01
            = fs00.cons (flt ("Delta 0121", "LGA", "MSP", 1100, 1409));
        RacketList<Flight> fs02
            = fs01.cons (flt ("Delta 0121", "LGA", "MSP", 1100, 1409));
        RacketList<Flight> fs03
            = fs02.cons (flt ("Delta 2163", "MSP", "PDX", 1500, 1902));
        RacketList<Flight> fs04
            = fs03.cons (flt ("Delta 2079", "BOS", "DTW", 1035, 1259));
        RacketList<Flight> fs05
            = fs04.cons (flt ("Delta 1523", "BOS", "DTW", 2158,   20));
        RacketList<Flight> fs06
            = fs05.cons (flt ("Delta 0058", "BOS", "LHR",   44,  720));
        RacketList<Flight> fs07
            = fs06.cons (flt ("Delta 2531", "BOS", "LAX", 1317, 2020));
        RacketList<Flight> fs08
            = fs07.cons (flt ("Delta 2532", "BOS", "LAX", 2250,  555));
        RacketList<Flight> fs09
            = fs08.cons (flt ("Delta 1959", "BOS", "MSP", 1050, 1417));
        RacketList<Flight> fs10
            = fs09.cons (flt ("Delta 1894", "BOS", "MSP", 1355, 1730));
        RacketList<Flight> fs11
            = fs10.cons (flt ("Delta 2391", "BOS", "MSP", 2135,  105));
        RacketList<Flight> fs12
            = fs11.cons (flt ("Delta 2734", "BOS", "LGA", 1100, 1230));
        RacketList<Flight> fs13
            = fs12.cons (flt ("Delta 3550", "BZN", "LAX", 2020, 2302));
        RacketList<Flight> fs14
            = fs13.cons (flt ("Delta 1601", "DEN", "DTW", 1305, 1611));
        RacketList<Flight> fs15
            = fs14.cons (flt ("Delta 0916", "DEN", "DTW", 2332,  219));
        RacketList<Flight> fs16
            = fs15.cons (flt ("Delta 0010", "DEN", "LHR", 2030,  945));
        RacketList<Flight> fs17
            = fs16.cons (flt ("Delta 5703", "DEN", "LAX", 1404, 1715));
        RacketList<Flight> fs18
            = fs17.cons (flt ("Delta 5743", "DEN", "LAX",   34,  331));
        RacketList<Flight> fs19
            = fs18.cons (flt ("Delta 2437", "DTW", "BOS", 1345, 1546));
        RacketList<Flight> fs20
            = fs19.cons (flt ("Delta 0158", "DTW", "BOS", 1700, 1855));
        RacketList<Flight> fs21
            = fs20.cons (flt ("Delta 1700", "DTW", "BOS", 2240,   42));
        RacketList<Flight> fs22
            = fs21.cons (flt ("Delta 1511", "DTW", "DEN", 1330, 1651));
        RacketList<Flight> fs23
            = fs22.cons (flt ("Delta 1645", "DTW", "DEN", 1711, 2038));
        RacketList<Flight> fs24
            = fs23.cons (flt ("Delta 1706", "DTW", "LAX", 1320, 1845));
        RacketList<Flight> fs25
            = fs24.cons (flt ("Delta 0249", "DTW", "MSP", 1500, 1707));
        RacketList<Flight> fs26
            = fs25.cons (flt ("Delta 2359", "DTW", "MSP", 1715, 1920));
        RacketList<Flight> fs27
            = fs26.cons (flt ("Delta 2476", "DTW", "MSP",  110,  318));
        RacketList<Flight> fs28
            = fs27.cons (flt ("Delta 0059", "LHR", "BOS",  920, 1726));
        RacketList<Flight> fs29
            = fs28.cons (flt ("Delta 4378", "LHR", "BOS", 1645,   20));
        RacketList<Flight> fs30
            = fs29.cons (flt ("Delta 0011", "LHR", "DEN", 1255,  220));
        RacketList<Flight> fs31
            = fs30.cons (flt ("Delta 0302", "LAX", "BOS", 1625, 2214));
        RacketList<Flight> fs32
            = fs31.cons (flt ("Delta 5732", "LAX", "BZN",   30,  318));
        RacketList<Flight> fs33
            = fs32.cons (flt ("Delta 4574", "LAX", "DEN", 1735, 2007));
        RacketList<Flight> fs34
            = fs33.cons (flt ("Delta 5700", "LAX", "DEN",   10,  245));
        RacketList<Flight> fs35
            = fs34.cons (flt ("Delta 2077", "LAX", "PDX", 1735, 2009));
        RacketList<Flight> fs36
            = fs35.cons (flt ("Delta 1728", "MSP", "BOS", 1600, 1851));
        RacketList<Flight> fs37
            = fs36.cons (flt ("Delta 2305", "MSP", "BZN",  221,  513));
        RacketList<Flight> fs38
            = fs37.cons (flt ("Delta 1609", "MSP", "DEN", 2035, 2252));
        RacketList<Flight> fs39
            = fs38.cons (flt ("Delta 1836", "MSP", "DTW", 1224, 1415));
        RacketList<Flight> fs40
            = fs39.cons (flt ("Delta 1734", "MSP", "DTW", 1755, 1941));
        RacketList<Flight> fs41
            = fs40.cons (flt ("Delta 0592", "MSP", "LGA", 1730, 2017));
        RacketList<Flight> fs42
            = fs41.cons (flt ("Delta 2734", "LGA", "BOS", 1100, 1208));
        RacketList<Flight> fs43
            = fs42.cons (flt ("Delta 1294", "LGA", "DEN", 1310, 1754));
        RacketList<Flight> fs44
            = fs43.cons (flt ("Delta 0879", "LGA", "DTW", 1410, 1620));
        RacketList<Flight> fs45
            = fs44.cons (flt ("Delta 1422", "LGA", "MSP", 1500, 1822));
        RacketList<Flight> fs46
            = fs45.cons (flt ("Delta 0950", "PDX", "LAX", 1418, 1655));
        RacketList<Flight> fs47
            = fs46.cons (flt ("Delta 2077", "PDX", "LAX", 2045, 2314));
        RacketList<Flight> fs48
            = fs47.cons (flt ("Delta 2831", "PDX", "LAX", 2346,  225));
        RacketList<Flight> fs49
            = fs48.cons (flt ("Delta 2167", "PDX", "MSP", 2200,  120));

        return fs49;
    }

    // Returns another list of flights extracted from schedules published
    // by Delta Airlines.

    private static RacketList<Flight> initDeltaCycle() {
        RacketList<Flight> fs00 = RacketLists.empty();
        RacketList<Flight> fs01
            = fs00.cons (flt ("Delta 0105", "BOS", "ATL", 1950, 2259));
        RacketList<Flight> fs02
            = fs01.cons (flt ("Delta 1895", "ATL", "PHL", 1505, 1705));
        RacketList<Flight> fs03
            = fs02.cons (flt ("Delta 0926", "PHL", "SLC", 1059, 1615));
        RacketList<Flight> fs04
            = fs03.cons (flt ("Delta 5828", "SLC", "DFW", 1813, 2056));
        RacketList<Flight> fs05
            = fs04.cons (flt ("Delta 8122", "DFW", "MEX",  132,  435));
        RacketList<Flight> fs06
            = fs05.cons (flt ("Delta 8028", "MEX", "LAS", 1800, 2228));
        RacketList<Flight> fs07
            = fs06.cons (flt ("Delta 2837", "LAS", "MKC",  215,  505));
        RacketList<Flight> fs08
            = fs07.cons (flt ("Delta 3337", "MKC", "ORL", 2000, 2250));
        RacketList<Flight> fs09
            = fs08.cons (flt ("Delta 3617", "ORL", "BNA", 1735, 1936));
        RacketList<Flight> fs10
            = fs09.cons (flt ("Delta 4811", "BNA", "CVG", 1215, 1333));
        RacketList<Flight> fs11
            = fs10.cons (flt ("Delta 6207", "CVG", "IAH", 1850, 2131));
        RacketList<Flight> fs12
            = fs11.cons (flt ("Delta 0108", "IAH", "MAD", 2006,  715));
        RacketList<Flight> fs13
            = fs12.cons (flt ("Delta 6775", "MAD", "MIA", 1425, 2350));
        RacketList<Flight> fs14
            = fs13.cons (flt ("Delta 7199", "MIA", "YTO", 2055,    6));
        RacketList<Flight> fs15
            = fs14.cons (flt ("Delta 7037", "YTO", "BOS", 2215,    5));

        return fs15;
    }
}
