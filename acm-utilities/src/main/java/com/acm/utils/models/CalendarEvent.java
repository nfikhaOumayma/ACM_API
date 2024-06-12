/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

// TODO: Auto-generated Javadoc
/**
 * {@link CalendarEvent} class.
 *
 * @author MoezMhiri
 * @since 0.5.0
 */
@Entity
@Table(name = "ACM_CALENDAR_EVENT")
public class CalendarEvent extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -257180715922948645L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CALENDAR_EVENT", unique = true, nullable = false)
	private Long id;

	/** The Date debut. */
	@Column(name = "DATE_DEBUT", nullable = false)
	private Date dateDebut;

	/** The Date fin. */
	@Column(name = "DATE_FIN", nullable = false)
	private Date dateFin;

	/** The type event. */
	@Column(name = "TYPE_EVENT", nullable = false)
	private String typeEvent;

	/** The libelle event. */
	@Column(name = "LIBELLE_EVENT", nullable = false)
	private String libelleEvent;

	/** The Description event. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The username. */
	@Column(name = "USERNAME", nullable = false)
	private String username;

	/** The name of customer. */
	@Column(name = "CUSTOMER_NAME", nullable = false)
	private String customerName;

	/** The phone number. */
	@Column(name = "PHONE_NUMBER", nullable = false)
	private String phoneNumer;

	/** The place. */
	@Column(name = "PLACE", nullable = false)
	private String place;

	/** The statut. */
	@Column(name = "STATUT", nullable = false)
	private String statut;

	/** The participant. */
	@Column(name = "PARTICIPANT", nullable = false)
	private String participant;

	/** The notifications. */
	@OneToMany(mappedBy = "calendarEvent", fetch = FetchType.LAZY)
	private Set<Notifications> notifications = new HashSet<>();

	/** The id loan extern. */
	@Column(name = "ID_LOAN_EXTERN")
	private Long idLoanExtern;

	/** The id collection. */
	@Column(name = "ACM_ID_COLLECTION")
	private Long idCollection;
	
	/** The id claim. */
	@Column(name = "ACM_ID_CLAIM")
	private Long idClaim;

	/** The category. */
	@Column(name = "CATEGORY", nullable = false)
	private String category;

	/** The step name. */
	@Column(name = "STEP_NAME")
	private String stepName;

	/** The user full name. */
	@Column(name = "USER_FULL_NAME")
	private String userFullName;

	/** The acm customer id. */
	@Column(name = "ID_CUSTOMER_EXTERN")
	private Long idCustomerExtern;

	/** The id item. */
	@Column(name = "ID_ACM_ITEM")
	private Long idItem;

	/** The acm conditionnal approves. */
	@JsonIgnore
	@OneToMany(mappedBy = "calendarEventApprove", fetch = FetchType.LAZY)
	private Set<AcmConditionnalApprove> acmConditionnalApproves = new HashSet<>();

	/** The acm conditionnal approves For Boss. */
	@JsonIgnore
	@OneToMany(mappedBy = "calendarEventApproveValidator", fetch = FetchType.LAZY,
			cascade = {CascadeType.MERGE, CascadeType.PERSIST})
	private Set<AcmConditionnalApprove> acmConditionnalApprovesVlidators = new HashSet<>();

	/**
	 * Instantiates a new CalendarEvent.
	 */
	public CalendarEvent() {

		/*
		 * Empty
		 */
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
	 * Gets the notifications.
	 *
	 * @return the notifications
	 */
	public Set<Notifications> getNotifications() {

		return notifications;
	}

	/**
	 * Sets the notifications.
	 *
	 * @param notifications the notifications to set
	 */
	public void setNotifications(Set<Notifications> notifications) {

		this.notifications = notifications;
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
	 * Sets the id loan extern.
	 *
	 * @param idLoanExtern the new id loan extern
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
	 * Gets the acm conditionnal approves.
	 *
	 * @return the acmConditionnalApproves
	 */
	public Set<AcmConditionnalApprove> getAcmConditionnalApproves() {

		return acmConditionnalApproves;
	}

	/**
	 * Sets the acm conditionnal approves.
	 *
	 * @param acmConditionnalApproves the acmConditionnalApproves to set
	 */
	public void setAcmConditionnalApproves(Set<AcmConditionnalApprove> acmConditionnalApproves) {

		this.acmConditionnalApproves = acmConditionnalApproves;
	}

	/**
	 * Gets the acm conditionnal approves vlidators.
	 *
	 * @return the acmConditionnalApprovesVlidators
	 */
	public Set<AcmConditionnalApprove> getAcmConditionnalApprovesVlidators() {

		return acmConditionnalApprovesVlidators;
	}

	/**
	 * Sets the acm conditionnal approves vlidators.
	 *
	 * @param acmConditionnalApprovesVlidators the acmConditionnalApprovesVlidators to set
	 */
	public void setAcmConditionnalApprovesVlidators(
			Set<AcmConditionnalApprove> acmConditionnalApprovesVlidators) {

		this.acmConditionnalApprovesVlidators = acmConditionnalApprovesVlidators;
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
