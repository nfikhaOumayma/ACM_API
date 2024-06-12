/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link SearchDecisionsResponse } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class SearchDecisionsResponse {

	/** The context. */
	public String context;

	/** The customer id. */
	public long customerId;

	/** The person id. */
	public long person_id;

	/** The search query id. */
	public Long search_query_id;

	/** The action. */
	public int action;

	/** The comment. */
	public String comment;

	/**
	 * Gets the context.
	 *
	 * @return the context
	 */
	public String getContext() {

		return context;
	}

	/**
	 * Sets the context.
	 *
	 * @param context the context to set
	 */
	public void setContext(String context) {

		this.context = context;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the person id.
	 *
	 * @return the person_id
	 */
	public long getPerson_id() {

		return person_id;
	}

	/**
	 * Sets the person id.
	 *
	 * @param person_id the person_id to set
	 */
	public void setPerson_id(long person_id) {

		this.person_id = person_id;
	}

	/**
	 * Gets the search query id.
	 *
	 * @return the search_query_id
	 */
	public Long getSearch_query_id() {

		return search_query_id;
	}

	/**
	 * Sets the search query id.
	 *
	 * @param search_query_id the search_query_id to set
	 */
	public void setSearch_query_id(Long search_query_id) {

		this.search_query_id = search_query_id;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public int getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the action to set
	 */
	public void setAction(int action) {

		this.action = action;
	}

	/**
	 * Gets the comment.
	 *
	 * @return the comment
	 */
	public String getComment() {

		return comment;
	}

	/**
	 * Sets the comment.
	 *
	 * @param comment the comment to set
	 */
	public void setComment(String comment) {

		this.comment = comment;
	}

}
