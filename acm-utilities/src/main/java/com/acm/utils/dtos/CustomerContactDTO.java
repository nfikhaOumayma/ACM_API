/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link CustomerContactDTO} class.
 *
 * @author Salmen Fatnssi
 * @since 0.17.0
 */
public class CustomerContactDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6494348981019060695L;

	/** The id. */
	private Long id;

	/** The from. */
	private String from;

	/** The to. */
	private String to;

	/** The subject. */
	private String subject;

	/** The content. */
	private String content;

	/** The link replay. */
	private Long linkReplay;

	/** The read. */
	private Boolean read;

	/** The priority. */
	private Integer priority;

	/** The statut. */
	private String statut;

	/** The customer id. */
	private Long customerId;

	/** The name. */
	private String name;

	/** The email. */
	private String email;

	/** The phone. */
	private String phone;

	/** The enabled. */
	private Boolean enabled;

	/** The sent. */
	private Boolean sentCustomer;

	/** The insert date. */
	private Date dateInsertion;

	/** The userName. */
	private String userName;

	/**
	 * Instantiates a new customer contact DTO.
	 */
	public CustomerContactDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer contact DTO.
	 *
	 * @param id the id
	 * @param from the from
	 * @param to the to
	 * @param subject the subject
	 * @param content the content
	 * @param linkReplay the link replay
	 * @param read the read
	 * @param priority the priority
	 * @param statut the statut
	 * @param customerId the customer id
	 * @param enabled the enabled
	 * @param sentCustomer the sent customer
	 * @param dateInsertion the date insertion
	 * @param userName the user name
	 */
	public CustomerContactDTO(Long id, String from, String to, String subject, String content,
			Long linkReplay, Boolean read, Integer priority, String statut, Long customerId,
			Boolean enabled, Boolean sentCustomer, Date dateInsertion, String userName) {

		this.id = id;
		this.from = from;
		this.to = to;
		this.subject = subject;
		this.content = content;
		this.linkReplay = linkReplay;
		this.read = read;
		this.priority = priority;
		this.statut = statut;
		this.customerId = customerId;
		this.enabled = enabled;
		this.sentCustomer = sentCustomer;
		this.dateInsertion = dateInsertion;
		this.userName = userName;
	}

	/**
	 * Instantiates a new customer contact DTO.
	 *
	 * @param to the to
	 * @param subject the subject
	 * @param content the content
	 * @param customerId the customer id
	 * @param sentCustomer the sent customer
	 */
	public CustomerContactDTO(String to, String subject, String content, Long customerId,
			Boolean sentCustomer) {

		this.to = to;
		this.subject = subject;
		this.content = content;
		this.customerId = customerId;
		this.sentCustomer = sentCustomer;
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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the from.
	 *
	 * @return the from
	 */
	public String getFrom() {

		return from;
	}

	/**
	 * Sets the from.
	 *
	 * @param from the new from
	 */
	public void setFrom(String from) {

		this.from = from;
	}

	/**
	 * Gets the to.
	 *
	 * @return the to
	 */
	public String getTo() {

		return to;
	}

	/**
	 * Sets the to.
	 *
	 * @param to the new to
	 */
	public void setTo(String to) {

		this.to = to;
	}

	/**
	 * Gets the subject.
	 *
	 * @return the subject
	 */
	public String getSubject() {

		return subject;
	}

	/**
	 * Sets the subject.
	 *
	 * @param subject the new subject
	 */
	public void setSubject(String subject) {

		this.subject = subject;
	}

	/**
	 * Gets the content.
	 *
	 * @return the content
	 */
	public String getContent() {

		return content;
	}

	/**
	 * Sets the content.
	 *
	 * @param content the new content
	 */
	public void setContent(String content) {

		this.content = content;
	}

	/**
	 * Gets the link replay.
	 *
	 * @return the link replay
	 */
	public Long getLinkReplay() {

		return linkReplay;
	}

	/**
	 * Sets the link replay.
	 *
	 * @param linkReplay the new link replay
	 */
	public void setLinkReplay(Long linkReplay) {

		this.linkReplay = linkReplay;
	}

	/**
	 * Gets the read.
	 *
	 * @return the read
	 */
	public Boolean getRead() {

		return read;
	}

	/**
	 * Sets the read.
	 *
	 * @param read the new read
	 */
	public void setRead(Boolean read) {

		this.read = read;
	}

	/**
	 * Gets the priority.
	 *
	 * @return the priority
	 */
	public Integer getPriority() {

		return priority;
	}

	/**
	 * Sets the priority.
	 *
	 * @param priority the new priority
	 */
	public void setPriority(Integer priority) {

		this.priority = priority;
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
	 * @param statut the new statut
	 */
	public void setStatut(String statut) {

		this.statut = statut;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customer id
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the new customer id
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the email.
	 *
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Sets the email.
	 *
	 * @param email the email to set
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the phone.
	 *
	 * @return the phone
	 */
	public String getPhone() {

		return phone;
	}

	/**
	 * Sets the phone.
	 *
	 * @param phone the phone to set
	 */
	public void setPhone(String phone) {

		this.phone = phone;
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
	 * Gets the sent customer.
	 *
	 * @return the sent customer
	 */
	public Boolean getSentCustomer() {

		return sentCustomer;
	}

	/**
	 * Sets the sent customer.
	 *
	 * @param sentCustomer the new sent customer
	 */
	public void setSentCustomer(Boolean sentCustomer) {

		this.sentCustomer = sentCustomer;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the user name.
	 *
	 * @return the user name
	 */
	public String getUserName() {

		return userName;
	}

	/**
	 * Sets the user name.
	 *
	 * @param userName the new user name
	 */
	public void setUserName(String userName) {

		this.userName = userName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerContactDTO [id=" + id + ", from=" + from + ", to=" + to + ", subject="
				+ subject + ", content=" + content + ", linkReplay=" + linkReplay + ", read=" + read
				+ ", priority=" + priority + ", statut=" + statut + ", customerId=" + customerId
				+ ", name=" + name + ", email=" + email + ", phone=" + phone + ", enabled="
				+ enabled + ", sentCustomer=" + sentCustomer + ", dateInsertion=" + dateInsertion
				+ ", userName=" + userName + "]";
	}

}
