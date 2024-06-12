/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

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
 * {@link SettingLevelProcess} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@Entity
@Table(name = "ACM_SETTING_LEVEL_PROCESS")
public class SettingLevelProcess extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5203328456280417284L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_LEVEL_PROCESS", unique = true, nullable = false)
	private Long id;

	/** The id product. */
	@Column(name = "ID_PRODUCT")
	private Long idProduct;

	/** The amount. */
	@Column(name = "AMOUNT")
	private BigDecimal amount;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The setting level. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_LEVEL")
	private SettingLevel settingLevel;

	/**
	 * Instantiates a new setting process level.
	 */
	public SettingLevelProcess() {

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
	 * Gets the id product.
	 *
	 * @return the idProduct
	 */
	public Long getIdProduct() {

		return idProduct;
	}

	/**
	 * Sets the id product.
	 *
	 * @param idProduct the idProduct to set
	 */
	public void setIdProduct(Long idProduct) {

		this.idProduct = idProduct;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
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
	 * Gets the setting level.
	 *
	 * @return the settingLevel
	 */
	public SettingLevel getSettingLevel() {

		return settingLevel;
	}

	/**
	 * Sets the setting level.
	 *
	 * @param settingLevel the settingLevel to set
	 */
	public void setSettingLevel(SettingLevel settingLevel) {

		this.settingLevel = settingLevel;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "SettingLevelProcess [id=" + id + ", idProduct=" + idProduct + ", amount=" + amount
				+ ", description=" + description + "]";
	}

}
