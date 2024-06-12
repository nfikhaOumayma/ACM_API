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
 * {@link ACM_RENEWAL_CONDITION} class.
 *
 * @author Ines Dridi
 * @since 1.1.3
 */
@Entity
@Table(name = "ACM_RENEWAL_CONDITION")
public class RenewalCondition extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2570280859470324044L;
	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_RENEWAL_CONDITION", unique = true, nullable = false)
	private Long id;

	/** The year. */
	@Column(name = "YEAR", nullable = false)
	private Integer year;

	/** The min amount. */
	@Column(name = "MIN_AMOUNT", nullable = false)
	private Long minAmount;

	/** The max amount. */
	@Column(name = "MAX_AMOUNT", nullable = false)
	private Long maxAmount;

	/** The pourcentage. */
	@Column(name = "POURCENTAGE")
	private Integer pourcentage;

	/** The ordre. */
	@Column(name = "ORDRE", nullable = false)
	private Long ordre;

	/**
	 * Instantiates a new renewal condition.
	 *
	 * @param id the id
	 * @param year the year
	 * @param minAmount the min amount
	 * @param maxAmount the max amount
	 * @param pourcentage the pourcentage
	 */
	public RenewalCondition(Long id, Integer year, Long minAmount, Long maxAmount,
			Integer pourcentage) {

		super();
		this.id = id;
		this.year = year;
		this.minAmount = minAmount;
		this.maxAmount = maxAmount;
		this.pourcentage = pourcentage;
	}

	/**
	 * Instantiates a new renewal condition.
	 */
	public RenewalCondition() {

		super();
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
	 * Gets the year.
	 *
	 * @return the year
	 */
	public Integer getYear() {

		return year;
	}

	/**
	 * Sets the year.
	 *
	 * @param year the new year
	 */
	public void setYear(Integer year) {

		this.year = year;
	}

	/**
	 * Gets the min amount.
	 *
	 * @return the min amount
	 */
	public Long getMinAmount() {

		return minAmount;
	}

	/**
	 * Sets the min amount.
	 *
	 * @param minAmount the new min amount
	 */
	public void setMinAmount(Long minAmount) {

		this.minAmount = minAmount;
	}

	/**
	 * Gets the max amount.
	 *
	 * @return the max amount
	 */
	public Long getMaxAmount() {

		return maxAmount;
	}

	/**
	 * Sets the max amount.
	 *
	 * @param maxAmount the new max amount
	 */
	public void setMaxAmount(Long maxAmount) {

		this.maxAmount = maxAmount;
	}

	/**
	 * Gets the pourcentage.
	 *
	 * @return the pourcentage
	 */
	public Integer getPourcentage() {

		return pourcentage;
	}

	/**
	 * Sets the pourcentage.
	 *
	 * @param pourcentage the new pourcentage
	 */
	public void setPourcentage(Integer pourcentage) {

		this.pourcentage = pourcentage;
	}

	/**
	 * Gets the ordre.
	 *
	 * @return the ordre
	 */
	public Long getOrdre() {

		return ordre;
	}

	/**
	 * Sets the ordre.
	 *
	 * @param ordre the new ordre
	 */
	public void setOrdre(Long ordre) {

		this.ordre = ordre;
	}

}
