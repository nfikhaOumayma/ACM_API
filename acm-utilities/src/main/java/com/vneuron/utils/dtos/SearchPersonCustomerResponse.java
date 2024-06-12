/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

import java.util.ArrayList;

/**
 * {@link SearchPersonCustomerResponse } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class SearchPersonCustomerResponse {

	/** The max score. */
	public double maxScore;

	/** The search result items. */
	public ArrayList<SearchResultItem> searchResultItems;

	/** The high risk nationality. */
	public boolean highRiskNationality;

	/** The high risk citizen ship. */
	public boolean highRiskCitizenShip;

	/** The comments. */
	public ArrayList<Object> comments;

	/** The search query id. */
	public Long search_query_id;

	/** The customer id. */
	public Long customer_id;

	/** The search rsult items. */
	public ArrayList<SearchResultItem> searchRsultItems;

	/**
	 * Gets the max score.
	 *
	 * @return the maxScore
	 */
	public double getMaxScore() {

		return maxScore;
	}

	/**
	 * Sets the max score.
	 *
	 * @param maxScore the maxScore to set
	 */
	public void setMaxScore(double maxScore) {

		this.maxScore = maxScore;
	}

	/**
	 * Gets the search result items.
	 *
	 * @return the searchResultItems
	 */
	public ArrayList<SearchResultItem> getSearchResultItems() {

		return searchResultItems;
	}

	/**
	 * Sets the search result items.
	 *
	 * @param searchResultItems the searchResultItems to set
	 */
	public void setSearchResultItems(ArrayList<SearchResultItem> searchResultItems) {

		this.searchResultItems = searchResultItems;
	}

	/**
	 * Checks if is high risk nationality.
	 *
	 * @return the highRiskNationality
	 */
	public boolean isHighRiskNationality() {

		return highRiskNationality;
	}

	/**
	 * Sets the high risk nationality.
	 *
	 * @param highRiskNationality the highRiskNationality to set
	 */
	public void setHighRiskNationality(boolean highRiskNationality) {

		this.highRiskNationality = highRiskNationality;
	}

	/**
	 * Checks if is high risk citizen ship.
	 *
	 * @return the highRiskCitizenShip
	 */
	public boolean isHighRiskCitizenShip() {

		return highRiskCitizenShip;
	}

	/**
	 * Sets the high risk citizen ship.
	 *
	 * @param highRiskCitizenShip the highRiskCitizenShip to set
	 */
	public void setHighRiskCitizenShip(boolean highRiskCitizenShip) {

		this.highRiskCitizenShip = highRiskCitizenShip;
	}

	/**
	 * Gets the comments.
	 *
	 * @return the comments
	 */
	public ArrayList<Object> getComments() {

		return comments;
	}

	/**
	 * Sets the comments.
	 *
	 * @param comments the comments to set
	 */
	public void setComments(ArrayList<Object> comments) {

		this.comments = comments;
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
	 * Gets the customer id.
	 *
	 * @return the customer_id
	 */
	public Long getCustomer_id() {

		return customer_id;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customer_id the customer_id to set
	 */
	public void setCustomer_id(Long customer_id) {

		this.customer_id = customer_id;
	}

	/**
	 * Gets the search rsult items.
	 *
	 * @return the searchRsultItems
	 */
	public ArrayList<SearchResultItem> getSearchRsultItems() {

		return searchRsultItems;
	}

	/**
	 * Sets the search rsult items.
	 *
	 * @param searchRsultItems the searchRsultItems to set
	 */
	public void setSearchRsultItems(ArrayList<SearchResultItem> searchRsultItems) {

		this.searchRsultItems = searchRsultItems;
	}

}
