/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CompanyResponse} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CompanyResponse {

	/** The report name. */
	@JsonProperty("ReportName")
	public String reportName;

	/** The report generation date. */
	@JsonProperty("ReportGenerationDate")
	public String reportGenerationDate;

	/** The c IR reference no. */
	@JsonProperty("CIRReferenceNo")
	public String cIRReferenceNo;

	/** The module data. */
	@JsonProperty("ModuleData")
	public List<ModuleData> moduleData;

	/** The p DF stream. */
	@JsonProperty("PDFStream")
	public byte[] pDFStream;

	/**
	 * Instantiates a new company response.
	 */
	public CompanyResponse() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CompanyResponse [" + (reportName != null ? "reportName=" + reportName + ", " : "")
				+ (reportGenerationDate != null
						? "reportGenerationDate=" + reportGenerationDate + ", "
						: "")
				+ (cIRReferenceNo != null ? "cIRReferenceNo=" + cIRReferenceNo + ", " : "")
				+ (moduleData != null ? "moduleData=" + moduleData + ", " : "")
				+ (pDFStream != null ? "pDFStream=" + Arrays.toString(pDFStream) : "") + "]";
	}

}
