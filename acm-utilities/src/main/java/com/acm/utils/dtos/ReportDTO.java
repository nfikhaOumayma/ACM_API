/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * {@link ReportDTO} class.
 *
 * @author MoezMhiri
 * @since 0.11.0
 */
public class ReportDTO extends GenericDTO implements Serializable {

	/**
	 * The Constant serialVersionUID.
	 */
	private static final long serialVersionUID = 4902509549774470261L;

	/** The inputFileName. */
	private String inputFileName;

	/** The type report. */
	private String typeReport;

	/** The params. */
	private Map<String, Object> params;

	/** The path where the report will be uploaded to in the GED. */
	private String path;

	/** The entryList. */
	private List<LoanDTO> entryList;

	/** The entry list collection. */
	private List<CollectionStepDTO> entryListCollection;

	/** The entry list acm collections. */
	private List<AcmCollectionDTO> entryListAcmCollections;

	/** The saveToGed, true when the report should be uploaded to GED, false otherwise. */
	private Boolean saveToGed;

	/** The tags to be attached to the document in the GED. */
	private List<String> tags = new ArrayList<>();

	/**
	 * Instantiates a new loan report DTO.
	 *
	 * @param inputFileName the input file name
	 * @param params the params
	 * @param path the path
	 * @param entryList the entry list
	 * @param saveToGed the save to ged
	 * @param tags the tags
	 */

	public ReportDTO(String inputFileName, Map<String, Object> params, String path,
			List<LoanDTO> entryList, Boolean saveToGed, List<String> tags) {

		this.inputFileName = inputFileName;
		this.params = params;
		this.path = path;
		this.entryList = entryList;
		this.saveToGed = saveToGed;
		this.tags = tags;
	}

	/**
	 * Instantiates a new report DTO.
	 */
	public ReportDTO() {

	}

	/**
	 * Gets the input file name.
	 *
	 * @return the inputFileName
	 */
	public String getInputFileName() {

		return inputFileName;
	}

	/**
	 * Sets the input file name.
	 *
	 * @param inputFileName the inputFileName to set
	 */
	public void setInputFileName(String inputFileName) {

		this.inputFileName = inputFileName;
	}

	/**
	 * Gets the params.
	 *
	 * @return the params
	 */
	public Map<String, Object> getParams() {

		return params;
	}

	/**
	 * Gets the type report.
	 *
	 * @return the type report
	 */
	public String getTypeReport() {

		return typeReport;
	}

	/**
	 * Gets the entry list collection.
	 *
	 * @return the entry list collection
	 */
	public List<CollectionStepDTO> getEntryListCollection() {

		return entryListCollection;
	}

	/**
	 * Sets the entry list collection.
	 *
	 * @param entryListCollection the new entry list collection
	 */
	public void setEntryListCollection(List<CollectionStepDTO> entryListCollection) {

		this.entryListCollection = entryListCollection;
	}

	/**
	 * Gets the entry list acm collections.
	 *
	 * @return the entry list acm collections
	 */
	public List<AcmCollectionDTO> getEntryListAcmCollections() {

		return entryListAcmCollections;
	}

	/**
	 * Sets the entry list acm collections.
	 *
	 * @param entryListAcmCollections the new entry list acm collections
	 */
	public void setEntryListAcmCollections(List<AcmCollectionDTO> entryListAcmCollections) {

		this.entryListAcmCollections = entryListAcmCollections;
	}

	/**
	 * Sets the type report.
	 *
	 * @param typeReport the new type report
	 */
	public void setTypeReport(String typeReport) {

		this.typeReport = typeReport;
	}

	/**
	 * Sets the params.
	 *
	 * @param params the params to set
	 */
	public void setParams(Map<String, Object> params) {

		this.params = params;
	}

	/**
	 * Gets the path.
	 *
	 * @return the path
	 */
	public String getPath() {

		return path;
	}

	/**
	 * Sets the path.
	 *
	 * @param path the path to set
	 */
	public void setPath(String path) {

		this.path = path;
	}

	/**
	 * Gets the entry list.
	 *
	 * @return the entryList
	 */
	public List<LoanDTO> getEntryList() {

		return entryList;
	}

	/**
	 * Sets the entry list.
	 *
	 * @param entryList the entryList to set
	 */
	public void setEntryList(List<LoanDTO> entryList) {

		this.entryList = entryList;
	}

	/**
	 * Gets the save to ged.
	 *
	 * @return the saveToGed
	 */
	public Boolean getSaveToGed() {

		return saveToGed;
	}

	/**
	 * Sets the save to ged.
	 *
	 * @param saveToGed the saveToGed to set
	 */
	public void setSaveToGed(Boolean saveToGed) {

		this.saveToGed = saveToGed;
	}

	/**
	 * Gets the tags.
	 *
	 * @return the tags
	 */
	public List<String> getTags() {

		return tags;
	}

	/**
	 * Sets the tags.
	 *
	 * @param tags the tags to set
	 */
	public void setTags(List<String> tags) {

		this.tags = tags;
	}

}
