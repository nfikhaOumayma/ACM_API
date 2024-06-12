/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.acm.utils.audit.AuditTrailListener;

/**
 * The persistent class for the ACM_LINKS_RELATIONSHIPS table. {@link CustomerLinksRelationship}
 * class.
 * 
 * @author YesserSomai
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_LINKS_RELATIONSHIPS")
@EntityListeners(AuditTrailListener.class)
public class CustomerLinksRelationship extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5065849964874615117L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LINKS_RELATIONSHIPS", unique = true, nullable = false)
	private Long id;

	/** The customer id. */
	@Column(name = "ID_ACM_CUSTOMER", nullable = false)
	private Long customerId;

	/** The link relationship type. */
	@Column(name = "LINK_RELATIONSHIPS_TYPE", length = 255)
	private String linkRelationshipType;

	/** The category. */
	@Column(name = "CATEGORY", length = 100)
	private String category;

	/** The date debut. */
	@Column(name = "DATE_DEBUT")
	private Date dateDebut;

	/** The date fin. */
	@Column(name = "DATE_FIN")
	private Date dateFin;

	/** The idloan. */
	@Column(name = "ID_ACM_LOAN ")
	private Long idLoan;

	/** The amount guarantor. */
	@Column(name = "AMOUNT_GUARANTOR  ")
	private BigDecimal amountGuarantor;

	/** The percentage owned. */
	@Column(name = "PERCENTAGE_OWNED  ")
	private BigDecimal percentageOwned;

	/** The member id. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_CUSTOMER_MEMBER")
	private Customer member;

	/**
	 * Instantiates a new customer links relationships.
	 */
	public CustomerLinksRelationship() {

		/*
		 * EMPTY
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
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the link relationship type.
	 *
	 * @return the linkRelationshipType
	 */
	public String getLinkRelationshipType() {

		return linkRelationshipType;
	}

	/**
	 * Sets the link relationship type.
	 *
	 * @param linkRelationshipType the linkRelationshipType to set
	 */
	public void setLinkRelationshipType(String linkRelationshipType) {

		this.linkRelationshipType = linkRelationshipType;
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
	 * Gets the date debut.
	 *
	 * @return the dateDebut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the date debut.
	 *
	 * @param dateDebut the dateDebut to set
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the date fin.
	 *
	 * @return the dateFin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the date fin.
	 *
	 * @param dateFin the dateFin to set
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
	}

	/**
	 * Gets the member.
	 *
	 * @return the member
	 */
	public Customer getMember() {

		return member;
	}

	/**
	 * Sets the member.
	 *
	 * @param member the member to set
	 */
	public void setMember(Customer member) {

		this.member = member;
	}

	/**
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the amount guarantor.
	 *
	 * @return the amountGuarantor
	 */
	public BigDecimal getAmountGuarantor() {

		return amountGuarantor;
	}

	/**
	 * Sets the amount guarantor.
	 *
	 * @param amountGuarantor the amountGuarantor to set
	 */
	public void setAmountGuarantor(BigDecimal amountGuarantor) {

		this.amountGuarantor = amountGuarantor;
	}

	/**
	 * Gets the percentage owned.
	 *
	 * @return the percentageOwned
	 */
	public BigDecimal getPercentageOwned() {

		return percentageOwned;
	}

	/**
	 * Sets the percentage owned.
	 *
	 * @param percentageOwned the percentageOwned to set
	 */
	public void setPercentageOwned(BigDecimal percentageOwned) {

		this.percentageOwned = percentageOwned;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerLinksRelationship [id=" + id + ", customerId=" + customerId
				+ ", linkRelationshipType=" + linkRelationshipType + ", category=" + category
				+ ", dateDebut=" + dateDebut + ", dateFin=" + dateFin + ", idLoan=" + idLoan
				+ ", amountGuarantor=" + amountGuarantor + ", member=" + member + "]";
	}
}
