/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link AcmConditionnalApprove} class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_CONDITIONNAL_APPROVE")
public class AcmConditionnalApprove extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2613414464895720494L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CONDITIONNAL_APPROVE", unique = true, nullable = false)
	private Long id;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ACM_ID_LOAN")
	private Loan loan;

	/** The user. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "USERNAME")
	private User user;

	/** The calendar event approve. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CALENDAR_EVENT")
	private CalendarEvent calendarEventApprove;

	/** The calendar event approve. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CALENDAR_EVENT_VALIDATOR")
	private CalendarEvent calendarEventApproveValidator;

	/** The status. */
	@Column(name = "STATUS")
	private String status;

	/** The conditionnal validation. */
	@Column(name = "CONDITIONNAL_VALIDATION")
	private Boolean conditionnalValidation;

	/** The username inserted by. */
	@Column(name = "USERNAME_INSERTED_BY")
	private String usernameInsertedBy;

	/** The approval condition date. */
	@Column(name = "APPROVAL_CONDITION_DATE")
	private Date approvalConditionDate;

	/** The item. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_ITEM")
	private Item item;

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
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the loan to set
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
	}

	/**
	 * Gets the user.
	 *
	 * @return the user
	 */
	public User getUser() {

		return user;
	}

	/**
	 * Sets the user.
	 *
	 * @param user the user to set
	 */
	public void setUser(User user) {

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
	 * Gets the calendar event approve.
	 *
	 * @return the calendarEventApprove
	 */
	public CalendarEvent getCalendarEventApprove() {

		return calendarEventApprove;
	}

	/**
	 * Sets the calendar event approve.
	 *
	 * @param calendarEventApprove the calendarEventApprove to set
	 */
	public void setCalendarEventApprove(CalendarEvent calendarEventApprove) {

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
	public CalendarEvent getCalendarEventApproveValidator() {

		return calendarEventApproveValidator;
	}

	/**
	 * Sets the calendar event approve validator.
	 *
	 * @param calendarEventApproveValidator the calendarEventApproveValidator to set
	 */
	public void setCalendarEventApproveValidator(CalendarEvent calendarEventApproveValidator) {

		this.calendarEventApproveValidator = calendarEventApproveValidator;
	}

	/**
	 * Gets the item.
	 *
	 * @return the item
	 */
	public Item getItem() {

		return item;
	}

	/**
	 * Sets the item.
	 *
	 * @param item the new item
	 */
	public void setItem(Item item) {

		this.item = item;
	}

}
