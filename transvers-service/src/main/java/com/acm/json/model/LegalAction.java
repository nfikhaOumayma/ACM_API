/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link LegalAction} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class LegalAction {

	/** The legal action details. */
	@JsonProperty("LegalActionDetails")
	public List<LegalActionDetail> legalActionDetails;

	/** The first legal action date. */
	@JsonProperty("First_Legal_Action_Date")
	public String first_Legal_Action_Date;

	/** The main action DESC. */
	@JsonProperty("MainAction_DESC")
	public String mainAction_DESC;

	/** The main action. */
	@JsonProperty("MainAction")
	public String mainAction;

	/** The sub action DESC. */
	@JsonProperty("SubAction_DESC")
	public String subAction_DESC;

	/** The sub action. */
	@JsonProperty("SubAction")
	public String subAction;

	/** The l EGA L STATEMEN T date. */
	@JsonProperty("LEGAL_STATEMENT_date")
	public String lEGAL_STATEMENT_date;

	/**
	 * Instantiates a new legal action.
	 */
	public LegalAction() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LegalAction ["
				+ (legalActionDetails != null ? "legalActionDetails=" + legalActionDetails + ", "
						: "")
				+ (first_Legal_Action_Date != null
						? "first_Legal_Action_Date=" + first_Legal_Action_Date + ", "
						: "")
				+ (mainAction_DESC != null ? "mainAction_DESC=" + mainAction_DESC + ", " : "")
				+ (mainAction != null ? "mainAction=" + mainAction + ", " : "")
				+ (subAction_DESC != null ? "subAction_DESC=" + subAction_DESC + ", " : "")
				+ (subAction != null ? "subAction=" + subAction + ", " : "")
				+ (lEGAL_STATEMENT_date != null ? "lEGAL_STATEMENT_date=" + lEGAL_STATEMENT_date
						: "")
				+ "]";
	}

}
