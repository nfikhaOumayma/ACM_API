/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos.pagination;

import java.io.Serializable;
import java.util.List;

import com.acm.utils.dtos.ExpensesDTO;

/**
 * {@link ExpensesPaginationDTO} class.
 * 
 * @author Ines Dridi
 * @since 1.1.3
 */
public class ExpensesPaginationDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6741191080805221621L;
	/** The page number. */
	private Integer pageNumber;

	/** The page size. */
	private Integer pageSize;

	/** The params. */
	private ExpensesDTO params;

	/** The sort direction. */
	private String sortDirection;

	/** The sort field. */
	private String sortField;

	/** The results loans. */
	private List<ExpensesDTO> resultsExpenses;

	/** The total pages. */
	private Integer totalPages;

	/** The total elements. */
	private Long totalElements;

	/**
	 * Instantiates a new expenses pagination DTO.
	 */
	public ExpensesPaginationDTO() {

	}

	/**
	 * Instantiates a new expenses pagination DTO.
	 *
	 * @param pageNumber the page number
	 * @param pageSize the page size
	 * @param params the params
	 */
	public ExpensesPaginationDTO(Integer pageNumber, Integer pageSize, ExpensesDTO params) {

		this.pageNumber = pageNumber;
		this.pageSize = pageSize;
		this.params = params;
	}

	/**
	 * Gets the page number.
	 *
	 * @return the page number
	 */
	public Integer getPageNumber() {

		return pageNumber;
	}

	/**
	 * Sets the page number.
	 *
	 * @param pageNumber the new page number
	 */
	public void setPageNumber(Integer pageNumber) {

		this.pageNumber = pageNumber;
	}

	/**
	 * Gets the page size.
	 *
	 * @return the page size
	 */
	public Integer getPageSize() {

		return pageSize;
	}

	/**
	 * Sets the page size.
	 *
	 * @param pageSize the new page size
	 */
	public void setPageSize(Integer pageSize) {

		this.pageSize = pageSize;
	}

	/**
	 * Gets the params.
	 *
	 * @return the params
	 */
	public ExpensesDTO getParams() {

		return params;
	}

	/**
	 * Sets the params.
	 *
	 * @param params the new params
	 */
	public void setParams(ExpensesDTO params) {

		this.params = params;
	}

	/**
	 * Gets the sort direction.
	 *
	 * @return the sort direction
	 */
	public String getSortDirection() {

		return sortDirection;
	}

	/**
	 * Sets the sort direction.
	 *
	 * @param sortDirection the new sort direction
	 */
	public void setSortDirection(String sortDirection) {

		this.sortDirection = sortDirection;
	}

	/**
	 * Gets the sort field.
	 *
	 * @return the sort field
	 */
	public String getSortField() {

		return sortField;
	}

	/**
	 * Sets the sort field.
	 *
	 * @param sortField the new sort field
	 */
	public void setSortField(String sortField) {

		this.sortField = sortField;
	}

	/**
	 * Gets the results expenses.
	 *
	 * @return the results expenses
	 */
	public List<ExpensesDTO> getResultsExpenses() {

		return resultsExpenses;
	}

	/**
	 * Sets the results expenses.
	 *
	 * @param resultsExpenses the new results expenses
	 */
	public void setResultsExpenses(List<ExpensesDTO> resultsExpenses) {

		this.resultsExpenses = resultsExpenses;
	}

	/**
	 * Gets the total pages.
	 *
	 * @return the total pages
	 */
	public Integer getTotalPages() {

		return totalPages;
	}

	/**
	 * Sets the total pages.
	 *
	 * @param totalPages the new total pages
	 */
	public void setTotalPages(Integer totalPages) {

		this.totalPages = totalPages;
	}

	/**
	 * Gets the total elements.
	 *
	 * @return the total elements
	 */
	public Long getTotalElements() {

		return totalElements;
	}

	/**
	 * Sets the total elements.
	 *
	 * @param totalElements the new total elements
	 */
	public void setTotalElements(Long totalElements) {

		this.totalElements = totalElements;
	}

}
