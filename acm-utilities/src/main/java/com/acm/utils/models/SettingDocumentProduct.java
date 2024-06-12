/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link SettingDocumentProduct} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Entity
@Table(name = "ACM_SETTING_DOC_PRODUCT")
public class SettingDocumentProduct extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2943320130539138256L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_DOC_PRODUCT", unique = true, nullable = false)
	private Long id;

	/** The setting document type. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_DOC_TYPE")
	private SettingDocumentType settingDocumentType;

	/** The product id. */
	@Column(name = "PRODUCT_ID", nullable = false)
	private Integer productId;

	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/** The date debut. */
	@Column(name = "DATE_DEBUT")
	private Date dateDebut;

	/** The date fin. */
	@Column(name = "DATE_FIN")
	private Date dateFin;

	/** The report name. */
	@Column(name = "REPORT_NAME")
	private String reportName;

	/** The collections. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "documents")
	private Set<CollectionStep> collections = new HashSet<>();

	/** The Loans. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "documents")
	private Set<WorkFlowStep> Loans = new HashSet<>();

	/**
	 * Gets the collections.
	 *
	 * @return the collections
	 */
	public Set<CollectionStep> getCollections() {

		return collections;
	}

	/**
	 * Gets the loans.
	 *
	 * @return the loans
	 */
	public Set<WorkFlowStep> getLoans() {

		return Loans;
	}

	/**
	 * Sets the loans.
	 *
	 * @param loans the new loans
	 */
	public void setLoans(Set<WorkFlowStep> loans) {

		Loans = loans;
	}

	/**
	 * Sets the collections.
	 *
	 * @param collections the new collections
	 */
	public void setCollections(Set<CollectionStep> collections) {

		this.collections = collections;
	}

	/**
	 * Instantiates a new setting document product.
	 */
	public SettingDocumentProduct() {

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
	 * Gets the setting document type.
	 *
	 * @return the settingDocumentType
	 */
	public SettingDocumentType getSettingDocumentType() {

		return settingDocumentType;
	}

	/**
	 * Sets the setting document type.
	 *
	 * @param settingDocumentType the settingDocumentType to set
	 */
	public void setSettingDocumentType(SettingDocumentType settingDocumentType) {

		this.settingDocumentType = settingDocumentType;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Integer getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Integer productId) {

		this.productId = productId;
	}

	/**
	 * Gets the mandatory.
	 *
	 * @return the mandatory
	 */
	public Boolean getMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
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
	 * Gets the report name.
	 *
	 * @return the reportName
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the reportName to set
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

}
