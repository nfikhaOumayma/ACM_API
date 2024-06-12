/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link AcmConditionnalApproveDTO} class.
 *
 * @author kouali
 * @since 0.1.0
 */

public class AcmConditionnalApproveDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2613414464895720494L;

	/** The id. */
	private Long id;

	/** The description. */
	private String description;

	/** The loan. */

	private LoanDTO loan;

	/** The user. */
	private UserDTO user;

	/** The calendar event id. */
	private Long calendarEventId;

	/** The status. */
	private String status;

	/** The insert by. */
	private String insertBy;

	/** The enabled. */
	private Boolean enabled;

	/** The calendar event approve. */
	private CalendarEventDTO calendarEventApprove;

	/** The calendar event approve validator. */
	private CalendarEventDTO calendarEventApproveValidator;

	/** The conditionnal validation. */
	private Boolean conditionnalValidation;

	/** The username inserted by. */
	private String usernameInsertedBy;

	/** The approval condition date. */
	private Date approvalConditionDate;

	/** The item. */
	private ItemDTO item;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public LoanDTO getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the loan to set
	 */
	public void setLoan(LoanDTO loan) {

		this.loan = loan;
	}

	/**
	 * Gets the user.
	 *
	 * @return the user
	 */
	public UserDTO getUser() {

		return user;
	}

	/**
	 * Sets the user.
	 *
	 * @param user the user to set
	 */
	public void setUser(UserDTO user) {

		this.user = user;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the calendar event id.
	 *
	 * @return the calendarEventId
	 */
	public Long getCalendarEventId() {

		return calendarEventId;
	}

	/**
	 * Sets the calendar event id.
	 *
	 * @param calendarEventId the calendarEventId to set
	 */
	public void setCalendarEventId(Long calendarEventId) {

		this.calendarEventId = calendarEventId;
	}

	/**
	 * Gets the calendar event approve.
	 *
	 * @return the calendarEventApprove
	 */
	public CalendarEventDTO getCalendarEventApprove() {

		return calendarEventApprove;
	}

	/**
	 * Sets the calendar event approve.
	 *
	 * @param calendarEventApprove the calendarEventApprove to set
	 */
	public void setCalendarEventApprove(CalendarEventDTO calendarEventApprove) {

		this.calendarEventApprove = calendarEventApprove;
	}

	/**
	 * Gets the conditionnal validation.
	 *
	 * @return the conditionnalValidation
	 */
	public Boolean getConditionnalValidation() {

		return conditionnalValidation;
	}

	/**
	 * Sets the conditionnal validation.
	 *
	 * @param conditionnalValidation the conditionnalValidation to set
	 */
	public void setConditionnalValidation(Boolean conditionnalValidation) {

		this.conditionnalValidation = conditionnalValidation;
	}

	/**
	 * Gets the username inserted by.
	 *
	 * @return the usernameInsertedBy
	 */
	public String getUsernameInsertedBy() {

		return usernameInsertedBy;
	}

	/**
	 * Sets the username inserted by.
	 *
	 * @param usernameInsertedBy the usernameInsertedBy to set
	 */
	public void setUsernameInsertedBy(String usernameInsertedBy) {

		this.usernameInsertedBy = usernameInsertedBy;
	}

	/**
	 * Gets the approval condition date.
	 *
	 * @return the approvalConditionDate
	 */
	public Date getApprovalConditionDate() {

		return approvalConditionDate;
	}

	/**
	 * Sets the approval condition date.
	 *
	 * @param approvalConditionDate the approvalConditionDate to set
	 */
	public void setApprovalConditionDate(Date approvalConditionDate) {

		this.approvalConditionDate = approvalConditionDate;
	}

	/**
	 * Gets the calendar event approve validator.
	 *
	 * @return the calendarEventApproveValidator
	 */
	public CalendarEventDTO getCalendarEventApproveValidator() {

		return calendarEventApproveValidator;
	}

	/**
	 * Sets the calendar event approve validator.
	 *
	 * @param calendarEventApproveValidator the calendarEventApproveValidator to set
	 */
	public void setCalendarEventApproveValidator(CalendarEventDTO calendarEventApproveValidator) {

		this.calendarEventApproveValidator = calendarEventApproveValidator;
	}

	/**
	 * Gets the item.
	 *
	 * @return the item
	 */
	public ItemDTO getItem() {

		return item;
	}

	/**
	 * Sets the item.
	 *
	 * @param item the new item
	 */
	public void setItem(ItemDTO item) {

		this.item = item;
	}

}
