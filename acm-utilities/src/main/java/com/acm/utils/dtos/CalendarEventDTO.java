/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

// TODO: Auto-generated Javadoc
/**
 * {@link CalendarEventDTO} class.
 *
 * @author MoezMhiri
 * @since 0.5.0
 */
public class CalendarEventDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7999771642585557801L;
	/** The id. */
	private Long id;

	/** The Date debut. */
	private Date dateDebut;

	/** The Date fin. */
	private Date dateFin;

	/** The type event. */
	private String typeEvent;

	/** The libelle event. */
	private String libelleEvent;

	/** The Description event. */
	private String description;

	/** The username. */
	private String username;

	/** The name of customer. */
	private String customerName;

	/** The phone number. */
	private String phoneNumer;

	/** The place. */
	private String place;

	/** The statut. */
	private String statut;

	/** The date insertion. */
	private Date dateInsertion;

	/** The participant. */
	private String participant;

	/** The sorted by date. */
	private Boolean sortedByDate;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The id collection. */
	private Long idCollection;
	
	/** The id claim. */
	private Long idClaim;

	/** The all teams tasks. */
	private Boolean allTeamsTasks; // true: get all the team's tasks;false:get only the user's tasks

	/** The category. */
	private String category;

	/** The user full name. */
	private String userFullName;

	/** The step name. */
	private String stepName;

	/** The insert by. */
	private String insertBy;

	/** The id customer extern. */
	private Long idCustomerExtern;

	/** The enabled. */
	private Boolean enabled;

	/** The user email. */
	private String userEmail;

	/** The full name participants. */
	private String fullNameParticipants;

	/** The customer number. */
	private String customerNumber;

	/** The id item. */
	private Long idItem;

	/**
	 * Instantiates a new CalendarEvent.
	 */
	public CalendarEventDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new CalendarEvent.
	 * 
	 * @param id the id
	 */
	public CalendarEventDTO(Long id) {

		this.id = id;

	}

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
	 * Gets the DateDebut.
	 * 
	 * @return the dateDebut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the DateDebut.
	 * 
	 * @param dateDebut the dateDebut to set
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the dateFin.
	 * 
	 * @return the dateFin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the dateFin.
	 * 
	 * @param dateFin the dateFin to set
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
	}

	/**
	 * Gets the typeEvent.
	 * 
	 * @return the typeEvent
	 */
	public String getTypeEvent() {

		return typeEvent;
	}

	/**
	 * Sets the typeEvent.
	 * 
	 * @param typeEvent the typeEvent to set
	 */
	public void setTypeEvent(String typeEvent) {

		this.typeEvent = typeEvent;
	}

	/**
	 * Gets the libelleEvent.
	 * 
	 * @return the libelleEvent
	 */
	public String getLibelleEvent() {

		return libelleEvent;
	}

	/**
	 * Sets the libelleEvent.
	 * 
	 * @param libelleEvent the libelleEvent to set
	 */
	public void setLibelleEvent(String libelleEvent) {

		this.libelleEvent = libelleEvent;
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
	 * Gets the customerName.
	 * 
	 * @return the customerName
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customerName.
	 * 
	 * @param customerName the customerName to set
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the phoneNumer.
	 * 
	 * @return the phoneNumer
	 */
	public String getPhoneNumer() {

		return phoneNumer;
	}

	/**
	 * Sets the phoneNumer.
	 * 
	 * @param phoneNumer the phoneNumer to set
	 */
	public void setPhoneNumer(String phoneNumer) {

		this.phoneNumer = phoneNumer;
	}

	/**
	 * Gets the place.
	 * 
	 * @return the place
	 */
	public String getPlace() {

		return place;
	}

	/**
	 * Sets the place.
	 * 
	 * @param place the place to set
	 */
	public void setPlace(String place) {

		this.place = place;
	}

	/**
	 * Gets the statut.
	 * 
	 * @return the statut
	 */
	public String getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 * 
	 * @param statut the statut to set
	 */
	public void setStatut(String statut) {

		this.statut = statut;
	}

	/**
	 * Gets the dateInsertion.
	 * 
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the dateInsertion.
	 * 
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the participant.
	 * 
	 * @return the participant
	 */
	public String getParticipant() {

		return participant;
	}

	/**
	 * Sets the participant.
	 * 
	 * @param participant the participant to set
	 */
	public void setParticipant(String participant) {

		this.participant = participant;
	}

	/**
	 * Gets the sorted by date.
	 *
	 * @return the sortedByDate
	 */
	public Boolean getSortedByDate() {

		return sortedByDate;
	}

	/**
	 * Sets the sorted by date.
	 *
	 * @param sortedByDate the sortedByDate to set
	 */
	public void setSortedByDate(Boolean sortedByDate) {

		this.sortedByDate = sortedByDate;
	}

	/**
	 * Gets the id loan extern.
	 *
	 * @return the id loan extern
	 */
	public Long getIdLoanExtern() {

		return idLoanExtern;
	}

	/**
	 * Sets the id lon extern.
	 *
	 * @param idLoanExtern the new id lon extern
	 */
	public void setIdLoanExtern(Long idLoanExtern) {

		this.idLoanExtern = idLoanExtern;
	}

	/**
	 * Gets the id collection.
	 *
	 * @return the id collection
	 */
	public Long getIdCollection() {

		return idCollection;
	}

	/**
	 * Sets the id collection.
	 *
	 * @param idCollection the new id collection
	 */
	public void setIdCollection(Long idCollection) {

		this.idCollection = idCollection;
	}

	/**
	 * Gets the all teams tasks.
	 *
	 * @return the all teams tasks
	 */
	public Boolean getAllTeamsTasks() {

		return allTeamsTasks;
	}

	/**
	 * Sets the all teams tasks.
	 *
	 * @param allTeamsTasks the new all teams tasks
	 */
	public void setAllTeamsTasks(Boolean allTeamsTasks) {

		this.allTeamsTasks = allTeamsTasks;
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
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the user full name.
	 *
	 * @return the user full name
	 */
	public String getUserFullName() {

		return userFullName;
	}

	/**
	 * Sets the user full name.
	 *
	 * @param userFullName the new user full name
	 */
	public void setUserFullName(String userFullName) {

		this.userFullName = userFullName;
	}

	/**
	 * Gets the step name.
	 *
	 * @return the step name
	 */
	public String getStepName() {

		return stepName;
	}

	/**
	 * Sets the step name.
	 *
	 * @param stepName the new step name
	 */
	public void setStepName(String stepName) {

		this.stepName = stepName;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the id customer extern.
	 *
	 * @return the id customer extern
	 */
	public Long getIdCustomerExtern() {

		return idCustomerExtern;
	}

	/**
	 * Sets the id customer extern.
	 *
	 * @param idCustomerExtern the new id customer extern
	 */
	public void setIdCustomerExtern(Long idCustomerExtern) {

		this.idCustomerExtern = idCustomerExtern;
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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the user email.
	 *
	 * @return the userEmail
	 */
	public String getUserEmail() {

		return userEmail;
	}

	/**
	 * Sets the user email.
	 *
	 * @param userEmail the userEmail to set
	 */
	public void setUserEmail(String userEmail) {

		this.userEmail = userEmail;
	}

	/**
	 * Gets the full name participants.
	 *
	 * @return the fullNameParticipants
	 */
	public String getFullNameParticipants() {

		return fullNameParticipants;
	}

	/**
	 * Sets the full name participants.
	 *
	 * @param fullNameParticipants the fullNameParticipants to set
	 */
	public void setFullNameParticipants(String fullNameParticipants) {

		this.fullNameParticipants = fullNameParticipants;
	}

	/**
	 * Gets the customer number.
	 *
	 * @return the customerNumber
	 */
	public String getCustomerNumber() {

		return customerNumber;
	}

	/**
	 * Sets the customer number.
	 *
	 * @param customerNumber the customerNumber to set
	 */
	public void setCustomerNumber(String customerNumber) {

		this.customerNumber = customerNumber;
	}
	
	
	/**
	 * Gets the id claim.
	 *
	 * @return the id claim
	 */
	public Long getIdClaim() {
		return idClaim;
	}

	/**
	 * Sets the id claim.
	 *
	 * @param idClaim the new id claim
	 */
	public void setIdClaim(Long idClaim) {
		this.idClaim = idClaim;
	}

	/**
	 * Gets the id item.
	 *
	 * @return the id item
	 */
	public Long getIdItem() {

		return idItem;
	}

	/**
	 * Sets the id item.
	 *
	 * @param idItem the new id item
	 */
	public void setIdItem(Long idItem) {

		this.idItem = idItem;
	}

}
