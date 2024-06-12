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
 * {@link Notifications} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
@Entity
@Table(name = "ACM_NOTIFICATION")
public class Notifications extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8760170974936183862L;

	/** The id notification. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_NOTIFICATION", unique = true, nullable = false)
	private Long idNotification;

	/** The username. */
	@Column(name = "USERNAME")
	private String username;

	/** The creation date. */
	@Column(name = "CREATION_DATE")
	private Date creactionDate;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The type motif. */
	@Column(name = "TYPE_NOTIF")
	private String typeMotif;

	/** The redirect. */
	@Column(name = "REDIRECT")
	private Boolean redirect;

	/** The status notif. */
	@Column(name = "STATUS_NOTIF")
	private String statusNotif;

	/** The action. */
	@Column(name = "ACTION")
	private String action;

	/** The title. */
	@Column(name = "TITLE")
	private String title;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/** The calendar event. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CALENDAR_EVENT")
	private CalendarEvent calendarEvent;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME")
	private String customerName;

	/** The target date. */
	@Column(name = "TARGET_DATE")
	private Date targetDate;

	/** The id acm collection. */
	@Column(name = "ID_ACM_COLLECTION")
	private Long idAcmCollection;

	/**
	 * Instantiates a new notifications.
	 */
	public Notifications() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the title.
	 *
	 * @return the title
	 */
	public String getTitle() {

		return title;
	}

	/**
	 * Sets the title.
	 *
	 * @param title the title to set
	 */
	public void setTitle(String title) {

		this.title = title;
	}

	/**
	 * Gets the id notification.
	 *
	 * @return the idNotification
	 */
	public Long getIdNotification() {

		return idNotification;
	}

	/**
	 * Sets the id notification.
	 *
	 * @param idNotification the idNotification to set
	 */
	public void setIdNotification(Long idNotification) {

		this.idNotification = idNotification;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the username to set
	 */
	public void setUsername(String username) {

		this.username = username;
	}

	/**
	 * Gets the creation date.
	 *
	 * @return the creactionDate
	 */
	public Date getCreactionDate() {

		return creactionDate;
	}

	/**
	 * Sets the creation date.
	 *
	 * @param creactionDate the creactionDate to set
	 */
	public void setCreactionDate(Date creactionDate) {

		this.creactionDate = creactionDate;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the type motif.
	 *
	 * @return the typeMotif
	 */
	public String getTypeMotif() {

		return typeMotif;
	}

	/**
	 * Sets the type motif.
	 *
	 * @param typeMotif the typeMotif to set
	 */
	public void setTypeMotif(String typeMotif) {

		this.typeMotif = typeMotif;
	}

	/**
	 * Gets the redirect.
	 *
	 * @return the redirect
	 */
	public Boolean getRedirect() {

		return redirect;
	}

	/**
	 * Sets the redirect.
	 *
	 * @param redirect the redirect to set
	 */
	public void setRedirect(Boolean redirect) {

		this.redirect = redirect;
	}

	/**
	 * Gets the status notif.
	 *
	 * @return the statusNotif
	 */
	public String getStatusNotif() {

		return statusNotif;
	}

	/**
	 * Sets the status notif.
	 *
	 * @param statusNotif the statusNotif to set
	 */
	public void setStatusNotif(String statusNotif) {

		this.statusNotif = statusNotif;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the action to set
	 */
	public void setAction(String action) {

		this.action = action;
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
	 * Gets the calendar event.
	 *
	 * @return the calendarEvent
	 */
	public CalendarEvent getCalendarEvent() {

		return calendarEvent;
	}

	/**
	 * Sets the calendar event.
	 *
	 * @param calendarEvent the calendarEvent to set
	 */
	public void setCalendarEvent(CalendarEvent calendarEvent) {

		this.calendarEvent = calendarEvent;
	}

	/**
	 * Instantiates a new notifications.
	 *
	 * @param idNotification the id notification
	 * @param username the username
	 * @param creactionDate the creaction date
	 * @param category the category
	 * @param typeMotif the type motif
	 * @param redirect the redirect
	 * @param statusNotif the status notif
	 * @param action the action
	 * @param title the title
	 * @param description the description
	 * @param loan the loan
	 * @param calendarEvent the calendar event
	 * @param customerName the customer name
	 * @param targetDate the target date
	 * @param idAcmCollection the id acm collection
	 */
	public Notifications(Long idNotification, String username, Date creactionDate, String category,
			String typeMotif, Boolean redirect, String statusNotif, String action, String title,
			String description, Loan loan, CalendarEvent calendarEvent, String customerName,
			Date targetDate, Long idAcmCollection) {

		super();
		this.idNotification = idNotification;
		this.username = username;
		this.creactionDate = creactionDate;
		this.category = category;
		this.typeMotif = typeMotif;
		this.redirect = redirect;
		this.statusNotif = statusNotif;
		this.action = action;
		this.title = title;
		this.description = description;
		this.loan = loan;
		this.calendarEvent = calendarEvent;
		this.customerName = customerName;
		this.targetDate = targetDate;
		this.idAcmCollection = idAcmCollection;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customer name
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the new customer name
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the target date.
	 *
	 * @return the target date
	 */
	public Date getTargetDate() {

		return targetDate;
	}

	/**
	 * Sets the target date.
	 *
	 * @param targetDate the new target date
	 */
	public void setTargetDate(Date targetDate) {

		this.targetDate = targetDate;
	}

	/**
	 * Gets the id acm collection.
	 *
	 * @return the id acm collection
	 */
	public Long getIdAcmCollection() {

		return idAcmCollection;
	}

	/**
	 * Sets the id acm collection.
	 *
	 * @param idAcmCollection the new id acm collection
	 */
	public void setIdAcmCollection(Long idAcmCollection) {

		this.idAcmCollection = idAcmCollection;
	}

}
