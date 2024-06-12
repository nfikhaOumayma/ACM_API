/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos.pagination;

import java.io.Serializable;
import java.util.List;

import com.acm.utils.dtos.AcmDocumentsDTO;

/**
 * {@link AcmDocumentsPaginationDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public class AcmDocumentsPaginationDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5386708382549419310L;

	/** The page number. */
	private Integer pageNumber;

	/** The page size. */
	private Integer pageSize;

	/** The params. */
	private AcmDocumentsDTO params;

	/** The sort direction. */
	private String sortDirection;

	/** The results acm documents. */
	private List<AcmDocumentsDTO> resultsAcmDocuments;

	/** The total pages. */
	private Integer totalPages;

	/** The total elements. */
	private Long totalElements;

	/**
	 * Instantiates a new acm documents pagination DTO.
	 */
	public AcmDocumentsPaginationDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm documents pagination DTO.
	 *
	 * @param pageNumber the page number
	 * @param pageSize the page size
	 * @param params the params
	 */
	public AcmDocumentsPaginationDTO(Integer pageNumber, Integer pageSize, AcmDocumentsDTO params) {

		this.pageNumber = pageNumber;
		this.pageSize = pageSize;
		this.params = params;
	}

	/**
	 * Gets the page number.
	 *
	 * @return the pageNumber
	 */
	public Integer getPageNumber() {

		return pageNumber;
	}

	/**
	 * Sets the page number.
	 *
	 * @param pageNumber the pageNumber to set
	 */
	public void setPageNumber(Integer pageNumber) {

		this.pageNumber = pageNumber;
	}

	/**
	 * Gets the page size.
	 *
	 * @return the pageSize
	 */
	public Integer getPageSize() {

		return pageSize;
	}

	/**
	 * Sets the page size.
	 *
	 * @param pageSize the pageSize to set
	 */
	public void setPageSize(Integer pageSize) {

		this.pageSize = pageSize;
	}

	/**
	 * Gets the params.
	 *
	 * @return the params
	 */
	public AcmDocumentsDTO getParams() {

		return params;
	}

	/**
	 * Sets the params.
	 *
	 * @param params the params to set
	 */
	public void setParams(AcmDocumentsDTO params) {

		this.params = params;
	}

	/**
	 * Gets the sort direction.
	 *
	 * @return the sortDirection
	 */
	public String getSortDirection() {

		return sortDirection;
	}

	/**
	 * Sets the sort direction.
	 *
	 * @param sortDirection the sortDirection to set
	 */
	public void setSortDirection(String sortDirection) {

		this.sortDirection = sortDirection;
	}

	/**
	 * Gets the total pages.
	 *
	 * @return the totalPages
	 */
	public Integer getTotalPages() {

		return totalPages;
	}

	/**
	 * Sets the total pages.
	 *
	 * @param totalPages the totalPages to set
	 */
	public void setTotalPages(Integer totalPages) {

		this.totalPages = totalPages;
	}

	/**
	 * Gets the total elements.
	 *
	 * @return the totalElements
	 */
	public Long getTotalElements() {

		return totalElements;
	}

	/**
	 * Sets the total elements.
	 *
	 * @param totalElements the totalElements to set
	 */
	public void setTotalElements(Long totalElements) {

		this.totalElements = totalElements;
	}

	/**
	 * Gets the results acm documents.
	 *
	 * @return the resultsAcmDocumentss
	 */
	public List<AcmDocumentsDTO> getResultsAcmDocuments() {

		return resultsAcmDocuments;
	}

	/**
	 * Sets the results acm documents.
	 *
	 * @param resultsAcmDocuments the new results acm documents
	 */
	public void setResultsAcmDocuments(List<AcmDocumentsDTO> resultsAcmDocuments) {

		this.resultsAcmDocuments = resultsAcmDocuments;
	}
}
