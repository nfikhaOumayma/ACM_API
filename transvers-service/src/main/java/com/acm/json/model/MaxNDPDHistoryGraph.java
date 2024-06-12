/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link MaxNDPDHistoryGraph} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class MaxNDPDHistoryGraph {

	/** The period id. */
	@JsonProperty("PeriodId")
	public String periodId;

	/** The m A X NU M DAY S DUE. */
	@JsonProperty("MAX_NUM_DAYS_DUE")
	public String mAX_NUM_DAYS_DUE;

	/** The r EPORTE D DATE. */
	@JsonProperty("REPORTED_DATE")
	public String rEPORTED_DATE;

	/**
	 * Instantiates a new max NDPD history graph.
	 */
	public MaxNDPDHistoryGraph() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "MaxNDPDHistoryGraph [" + (periodId != null ? "periodId=" + periodId + ", " : "")
				+ (mAX_NUM_DAYS_DUE != null ? "mAX_NUM_DAYS_DUE=" + mAX_NUM_DAYS_DUE + ", " : "")
				+ (rEPORTED_DATE != null ? "rEPORTED_DATE=" + rEPORTED_DATE : "") + "]";
	}

}
