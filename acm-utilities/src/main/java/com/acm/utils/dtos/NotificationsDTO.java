/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import org.dozer.Mapping;

/**
 * {@link NotificationsDTO} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
public class NotificationsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3682355475722727953L;

	/** The id notification. */
	private Long idNotification;

	/** The username. */
	private String username;

	/** The creaction date. */
	private Date creactionDate;

	/** The title. */
	private String title;

	/** The category. */
	private String category;

	/** The type motif. */
	private String typeMotif;

	/** The redirect. */
	private Boolean redirect;

	/** The status notif. */
	private String statusNotif;

	/** The action. */
	private String action;

	/** The description. */
	private String description;

	/** The loan DTO. */
	@Mapping("loan")
	private LoanDTO loanDTO;

	/** The calendar event DTO. */
	@Mapping("calendarEvent")
	private CalendarEventDTO calendarEventDTO;

	/** The action statue notif. */
	private Boolean actionStatueNotif;

	/** The insert by. */
	private String insertBy;

	/** The id acm collection. */
	private Long idAcmCollection;

	/** The customer name. */
	private String customerName;

	/** The target date. */
	private Date targetDate;

	/**
	 * Instantiates a new notifications DTO.
	 */
	public NotificationsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new notifications DTO.
	 *
	 * @param username the username
	 * @param category the category
	 * @param typeMotif the type motif
	 * @param redirect the redirect
	 * @param action the action
	 * @param description the description
	 * @param loanDTO the loan DTO
	 * @param calendarEventDTO the calendar event DTO
	 */
	public NotificationsDTO(String username, String category, String typeMotif, Boolean redirect,
			String action, String description, LoanDTO loanDTO, CalendarEventDTO calendarEventDTO) {

		this.username = username;
		this.category = category;
		this.typeMotif = typeMotif;
		this.redirect = redirect;
		this.action = action;
		this.description = description;
		this.loanDTO = loanDTO;
		this.calendarEventDTO = calendarEventDTO;
	}

	/**
	 * Instantiates a new notifications DTO.
	 *
	 * @param username the username
	 * @param category the category
	 * @param typeMotif the type motif
	 * @param redirect the redirect
	 * @param action the action
	 * @param description the description
	 * @param collectionId the collection id
	 */
	public NotificationsDTO(String username, String category, String typeMotif, Boolean redirect,
			String action, String description, Long collectionId) {

		this.username = username;
		this.category = category;
		this.typeMotif = typeMotif;
		this.redirect = redirect;
		this.action = action;
		this.description = description;
		this.idAcmCollection = collectionId;
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
	 * Gets the creaction date.
	 *
	 * @return the creactionDate
	 */
	public Date getCreactionDate() {

		return creactionDate;
	}

	/**
	 * Sets the creaction date.
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
	 * Gets the loan DTO.
	 *
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the calendar event DTO.
	 *
	 * @return the calendarEventDTO
	 */
	public CalendarEventDTO getCalendarEventDTO() {

		return calendarEventDTO;
	}

	/**
	 * Sets the calendar event DTO.
	 *
	 * @param calendarEventDTO the calendarEventDTO to set
	 */
	public void setCalendarEventDTO(CalendarEventDTO calendarEventDTO) {

		this.calendarEventDTO = calendarEventDTO;
	}

	/**
	 * Gets the action statue notif.
	 *
	 * @return the actionStatueNotif
	 */
	public Boolean getActionStatueNotif() {

		return actionStatueNotif;
	}

	/**
	 * Sets the action statue notif.
	 *
	 * @param actionStatueNotif the actionStatueNotif to set
	 */
	public void setActionStatueNotif(Boolean actionStatueNotif) {

		this.actionStatueNotif = actionStatueNotif;
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
	 * Instantiates a new notifications DTO.
	 *
	 * @param username the username
	 * @param title the title
	 * @param category the category
	 * @param typeMotif the type motif
	 * @param redirect the redirect
	 * @param action the action
	 * @param description the description
	 * @param customerName the customer name
	 * @param targetDate the target date
	 * @param idAcmCollection the id acm collection
	 */
	public NotificationsDTO(String username, String title, String category, String typeMotif,
			Boolean redirect, String action, String description, String customerName,
			Date targetDate, Long idAcmCollection) {

		super();
		this.username = username;
		this.title = title;
		this.category = category;
		this.typeMotif = typeMotif;
		this.redirect = redirect;
		this.action = action;
		this.description = description;
		this.customerName = customerName;
		this.targetDate = targetDate;
		this.idAcmCollection = idAcmCollection;
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
