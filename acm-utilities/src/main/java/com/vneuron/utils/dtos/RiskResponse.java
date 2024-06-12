/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

import java.util.ArrayList;
import java.util.Date;

import com.acm.utils.models.Customer;

/**
 * {@link RiskResponse} class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class RiskResponse {

	/** The id. */
	public int id;

	/** The risk level. */
	public RiskLevel riskLevel;

	/** The list value calculation result. */
	public ArrayList<ListValueCalculationResult> listValueCalculationResult;

	/** The risk calculation rate. */
	public double riskCalculationRate;

	/** The created on. */
	public Date createdOn;

	/** The rule. */
	public Object rule;

	/** The override request. */
	public Object overrideRequest;

	/** The customer. */
	public Customer customer;

	/** The user id. */
	public int userId;

	/** The user first name. */
	public String userFirstName;

	/** The user last name. */
	public String userLastName;

	/** The original value. */
	public Object originalValue;

	/** The overrided value. */
	public Object overridedValue;

	/** The eligible. */
	public Object eligible;

	/** The overrited. */
	public boolean overrited;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public int getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(int id) {

		this.id = id;
	}

	/**
	 * Gets the risk level.
	 *
	 * @return the riskLevel
	 */
	public RiskLevel getRiskLevel() {

		return riskLevel;
	}

	/**
	 * Sets the risk level.
	 *
	 * @param riskLevel the riskLevel to set
	 */
	public void setRiskLevel(RiskLevel riskLevel) {

		this.riskLevel = riskLevel;
	}

	/**
	 * Gets the list value calculation result.
	 *
	 * @return the listValueCalculationResult
	 */
	public ArrayList<ListValueCalculationResult> getListValueCalculationResult() {

		return listValueCalculationResult;
	}

	/**
	 * Sets the list value calculation result.
	 *
	 * @param listValueCalculationResult the listValueCalculationResult to set
	 */
	public void setListValueCalculationResult(
			ArrayList<ListValueCalculationResult> listValueCalculationResult) {

		this.listValueCalculationResult = listValueCalculationResult;
	}

	/**
	 * Gets the risk calculation rate.
	 *
	 * @return the riskCalculationRate
	 */
	public double getRiskCalculationRate() {

		return riskCalculationRate;
	}

	/**
	 * Sets the risk calculation rate.
	 *
	 * @param riskCalculationRate the riskCalculationRate to set
	 */
	public void setRiskCalculationRate(double riskCalculationRate) {

		this.riskCalculationRate = riskCalculationRate;
	}

	/**
	 * Gets the created on.
	 *
	 * @return the createdOn
	 */
	public Date getCreatedOn() {

		return createdOn;
	}

	/**
	 * Sets the created on.
	 *
	 * @param createdOn the createdOn to set
	 */
	public void setCreatedOn(Date createdOn) {

		this.createdOn = createdOn;
	}

	/**
	 * Gets the rule.
	 *
	 * @return the rule
	 */
	public Object getRule() {

		return rule;
	}

	/**
	 * Sets the rule.
	 *
	 * @param rule the rule to set
	 */
	public void setRule(Object rule) {

		this.rule = rule;
	}

	/**
	 * Gets the override request.
	 *
	 * @return the overrideRequest
	 */
	public Object getOverrideRequest() {

		return overrideRequest;
	}

	/**
	 * Sets the override request.
	 *
	 * @param overrideRequest the overrideRequest to set
	 */
	public void setOverrideRequest(Object overrideRequest) {

		this.overrideRequest = overrideRequest;
	}

	/**
	 * Gets the customer.
	 *
	 * @return the customer
	 */
	public Customer getCustomer() {

		return customer;
	}

	/**
	 * Sets the customer.
	 *
	 * @param customer the customer to set
	 */
	public void setCustomer(Customer customer) {

		this.customer = customer;
	}

	/**
	 * Gets the user id.
	 *
	 * @return the userId
	 */
	public int getUserId() {

		return userId;
	}

	/**
	 * Sets the user id.
	 *
	 * @param userId the userId to set
	 */
	public void setUserId(int userId) {

		this.userId = userId;
	}

	/**
	 * Gets the user first name.
	 *
	 * @return the userFirstName
	 */
	public String getUserFirstName() {

		return userFirstName;
	}

	/**
	 * Sets the user first name.
	 *
	 * @param userFirstName the userFirstName to set
	 */
	public void setUserFirstName(String userFirstName) {

		this.userFirstName = userFirstName;
	}

	/**
	 * Gets the user last name.
	 *
	 * @return the userLastName
	 */
	public String getUserLastName() {

		return userLastName;
	}

	/**
	 * Sets the user last name.
	 *
	 * @param userLastName the userLastName to set
	 */
	public void setUserLastName(String userLastName) {

		this.userLastName = userLastName;
	}

	/**
	 * Gets the original value.
	 *
	 * @return the originalValue
	 */
	public Object getOriginalValue() {

		return originalValue;
	}

	/**
	 * Sets the original value.
	 *
	 * @param originalValue the originalValue to set
	 */
	public void setOriginalValue(Object originalValue) {

		this.originalValue = originalValue;
	}

	/**
	 * Gets the overrided value.
	 *
	 * @return the overridedValue
	 */
	public Object getOverridedValue() {

		return overridedValue;
	}

	/**
	 * Sets the overrided value.
	 *
	 * @param overridedValue the overridedValue to set
	 */
	public void setOverridedValue(Object overridedValue) {

		this.overridedValue = overridedValue;
	}

	/**
	 * Gets the eligible.
	 *
	 * @return the eligible
	 */
	public Object getEligible() {

		return eligible;
	}

	/**
	 * Sets the eligible.
	 *
	 * @param eligible the eligible to set
	 */
	public void setEligible(Object eligible) {

		this.eligible = eligible;
	}

	/**
	 * Checks if is overrited.
	 *
	 * @return the overrited
	 */
	public boolean isOverrited() {

		return overrited;
	}

	/**
	 * Sets the overrited.
	 *
	 * @param overrited the overrited to set
	 */
	public void setOverrited(boolean overrited) {

		this.overrited = overrited;
	}

}
