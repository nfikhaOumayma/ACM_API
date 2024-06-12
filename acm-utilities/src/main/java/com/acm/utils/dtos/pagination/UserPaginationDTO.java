/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos.pagination;

import java.io.Serializable;
import java.util.List;

import com.acm.utils.dtos.UserDTO;

/**
 * {@link UserPaginationDTO} class.
 *
 * @author salman fatnassi
 * @since 1.1.1
 */
public class UserPaginationDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 9067880586708214226L;

	/** The page number. */
	private Integer pageNumber;

	/** The page size. */
	private Integer pageSize;

	/** The params. */
	private UserDTO params;

	/** The sort direction. */
	private String sortDirection;

	/** The sort field. */
	private String sortField;

	/** The results users. */
	private List<UserDTO> resultsUsers;

	/** The total pages. */
	private Integer totalPages;

	/** The total elements. */
	private Long totalElements;

	/**
	 * Instantiates a new user pagination DTO.
	 */
	public UserPaginationDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user pagination DTO.
	 *
	 * @param pageNumber the page number
	 * @param pageSize the page size
	 * @param params the params
	 */
	public UserPaginationDTO(Integer pageNumber, Integer pageSize, UserDTO params) {

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
	public UserDTO getParams() {

		return params;
	}

	/**
	 * Sets the params.
	 *
	 * @param params the new params
	 */
	public void setParams(UserDTO params) {

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
	 * Gets the results users.
	 *
	 * @return the results users
	 */
	public List<UserDTO> getResultsUsers() {

		return resultsUsers;
	}

	/**
	 * Sets the results users.
	 *
	 * @param resultsUsers the new results users
	 */
	public void setResultsUsers(List<UserDTO> resultsUsers) {

		this.resultsUsers = resultsUsers;
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
