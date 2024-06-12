/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos.pagination;

import java.io.Serializable;
import java.util.List;

import com.acm.utils.dtos.SupplierDTO;

/**
 * {@link SupplierPaginationDTO} class.
 *
 * @author KhaledOuali
 * @since 1.12
 */
public class SupplierPaginationDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8807111763924549568L;

	/** The page number. */
	private Integer pageNumber;

	/** The page size. */
	private Integer pageSize;

	/** The params. */
	private SupplierDTO params;

	/** The sort direction. */
	private String sortDirection;

	/** The sort field. */
	private String sortField;

	/** The results customers. */
	private List<SupplierDTO> resultsSupplier;

	/** The total pages. */
	private Integer totalPages;

	/** The total elements. */
	private Long totalElements;

	/**
	 * Instantiates a new loan pagination DTO.
	 */
	public SupplierPaginationDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan pagination DTO.
	 *
	 * @param pageNumber the page number
	 * @param pageSize the page size
	 * @param params the params
	 */
	public SupplierPaginationDTO(Integer pageNumber, Integer pageSize, SupplierDTO params) {

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
	public SupplierDTO getParams() {

		return params;
	}

	/**
	 * Sets the params.
	 *
	 * @param params the params to set
	 */
	public void setParams(SupplierDTO params) {

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
	 * Gets the sort field.
	 *
	 * @return the sortField
	 */
	public String getSortField() {

		return sortField;
	}

	/**
	 * Sets the sort field.
	 *
	 * @param sortField the sortField to set
	 */
	public void setSortField(String sortField) {

		this.sortField = sortField;
	}

	/**
	 * Gets the results customers.
	 *
	 * @return the resultsCustomers
	 */
	public List<SupplierDTO> getResultsSuppliers() {

		return resultsSupplier;
	}

	/**
	 * Sets the results customers.
	 *
	 * @param resultsSupplier the new results suppliers
	 */
	public void setResultsSuppliers(List<SupplierDTO> resultsSupplier) {

		this.resultsSupplier = resultsSupplier;
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

}
