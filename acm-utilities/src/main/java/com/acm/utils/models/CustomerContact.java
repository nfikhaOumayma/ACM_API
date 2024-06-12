/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The persistent class for the ACM_CUSTOMER_CONTACT table. {@link CustomerContact} class.
 * 
 * @author Salmen Fatnassi
 * @since 0.17.0
 */
@Entity
@Table(name = "ACM_CUSTOMER_CONTACT")
public class CustomerContact extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -42057143817834819L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CUSTOMER_CONTACT", unique = true, nullable = false)
	private Long id;

	/** The from. */
	@Column(name = "ACM_FROM", length = 512)
	private String from;

	/** The to. */
	@Column(name = "ACM_TO", length = 512)
	private String to;

	/** The subject. */
	@Column(name = "SUBJECT", length = 512)
	private String subject;

	/** The content. */
	@Column(name = "CONTENT", length = 1000)
	private String content;

	/** The link replay. */
	@Column(name = "LINK_REPLAY")
	private Long linkReplay;

	/** The read. */
	@Column(name = "READED")
	private Boolean read;

	/** The priority. */
	@Column(name = "PRIORITY")
	private Integer priority;

	/** The name. */
	@Column(name = "NAME")
	private String name;

	/** The email. */
	@Column(name = "EMAIL")
	private String email;

	/** The phone. */
	@Column(name = "PHONE")
	private String phone;

	/** The statut. */
	@Column(name = "STATUT", length = 256)
	private String statut;

	/** The customer id. */
	@Column(name = "ID_ACM_CUSTOMER")
	private Long customerId;

	/** The sent. */
	@Column(name = "SENT_CUSTOMER")
	private Boolean sentCustomer;

	/** The loan officer. */
	@Column(name = "USERNAME")
	private String userName;

	/**
	 * Instantiates a new customer contact.
	 */
	public CustomerContact() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer contact.
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
	 * @param sentCustomer the sent customer
	 * @param userName the user name
	 */
	public CustomerContact(Long id, String from, String to, String subject, String content,
			Long linkReplay, Boolean read, Integer priority, String statut, Long customerId,
			Boolean sentCustomer, String userName) {

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
		this.sentCustomer = sentCustomer;
		this.userName = userName;
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

		return "CustomerContact [id=" + id + ", from=" + from + ", to=" + to + ", subject="
				+ subject + ", content=" + content + ", linkReplay=" + linkReplay + ", read=" + read
				+ ", priority=" + priority + ", name=" + name + ", email=" + email + ", phone="
				+ phone + ", statut=" + statut + ", customerId=" + customerId + ", sentCustomer="
				+ sentCustomer + ", userName=" + userName + "]";
	}

}
