/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

import com.acm.utils.models.SettingGurantorCollateral;

/**
 * {@link SettingGurantorCollateral} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
public class SettingGurantorCollateralDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7598674207869167724L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Integer productId;

	/** The code. -- GUARANTOR || -- COLLATERAL. */
	private String code;

	/** The key abacus. -- cuLoanGuarantor || -- cuLoanCollateral. */
	private String keyAbacus;

	/** The description. */
	private String description;

	/** The mandatory. */
	private Boolean mandatory;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/**
	 * Instantiates a new setting gurantor collateral DTO.
	 */
	public SettingGurantorCollateralDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting gurantor collateral DTO.
	 *
	 * @param productId the product id
	 * @param mandatory the mandatory
	 */
	public SettingGurantorCollateralDTO(Integer productId, Boolean mandatory) {

		this.productId = productId;
		this.mandatory = mandatory;
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
