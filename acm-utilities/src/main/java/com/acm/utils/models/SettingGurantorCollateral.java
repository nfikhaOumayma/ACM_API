/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link SettingGurantorCollateral} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
@Entity
@Table(name = "ACM_SETTING_GUARANTOR_COLLATERAL")
public class SettingGurantorCollateral extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2715912519056636909L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_GUARANTOR_COLLATERAL", unique = true, nullable = false)
	private Long id;

	/** The product id. */
	@Column(name = "ID_PRODUCT", nullable = false)
	private Integer productId;

	/** The code. -- GUARANTOR || -- COLLATERAL. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The key abacus. -- cuLoanGuarantor || -- cuLoanCollateral. */
	@Column(name = "key_ABACUS", nullable = false)
	private String keyAbacus;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/** The date debut. */
	@Column(name = "DATE_DEBUT")
	private Date dateDebut;

	/** The date fin. */
	@Column(name = "DATE_FIN")
	private Date dateFin;

	/**
	 * Instantiates a new setting gurantor collateral.
	 */
	public SettingGurantorCollateral() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting gurantor collateral.
	 *
	 * @param id the id
	 */
	public SettingGurantorCollateral(Long id) {

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
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the key abacus.
	 *
	 * @return the keyAbacus
	 */
	public String getKeyAbacus() {

		return keyAbacus;
	}

	/**
	 * Sets the key abacus.
	 *
	 * @param keyAbacus the keyAbacus to set
	 */
	public void setKeyAbacus(String keyAbacus) {

		this.keyAbacus = keyAbacus;
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

}
