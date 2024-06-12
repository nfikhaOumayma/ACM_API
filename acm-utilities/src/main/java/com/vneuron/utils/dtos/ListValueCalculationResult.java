/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link ListValueCalculationResult } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class ListValueCalculationResult {

	/** The id. */
	public int id;

	/** The risk entity value. */
	public RiskEntityValue riskEntityValue;

	/** The risk level. */
	public RiskLevel riskLevel;

	/** The risk entity. */
	public RiskEntity riskEntity;

	/** The risk level source type. */
	public String riskLevelSourceType;

	/** The risk level source name. */
	public String riskLevelSourceName;

	/** The risk score. */
	public Object riskScore;

	/** The risk value calculation rate. */
	public int riskValueCalculationRate;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public int getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(int id) {

		this.id = id;
	}

	/**
	 * Gets the risk entity value.
	 *
	 * @return the riskEntityValue
	 */
	public RiskEntityValue getRiskEntityValue() {

		return riskEntityValue;
	}

	/**
	 * Sets the risk entity value.
	 *
	 * @param riskEntityValue the riskEntityValue to set
	 */
	public void setRiskEntityValue(RiskEntityValue riskEntityValue) {

		this.riskEntityValue = riskEntityValue;
	}

	/**
	 * Gets the risk level.
	 *
	 * @return the riskLevel
	 */
	public RiskLevel getRiskLevel() {

		return riskLevel;
	}

	/**
	 * Sets the risk level.
	 *
	 * @param riskLevel the riskLevel to set
	 */
	public void setRiskLevel(RiskLevel riskLevel) {

		this.riskLevel = riskLevel;
	}

	/**
	 * Gets the risk entity.
	 *
	 * @return the riskEntity
	 */
	public RiskEntity getRiskEntity() {

		return riskEntity;
	}

	/**
	 * Sets the risk entity.
	 *
	 * @param riskEntity the riskEntity to set
	 */
	public void setRiskEntity(RiskEntity riskEntity) {

		this.riskEntity = riskEntity;
	}

	/**
	 * Gets the risk level source type.
	 *
	 * @return the riskLevelSourceType
	 */
	public String getRiskLevelSourceType() {

		return riskLevelSourceType;
	}

	/**
	 * Sets the risk level source type.
	 *
	 * @param riskLevelSourceType the riskLevelSourceType to set
	 */
	public void setRiskLevelSourceType(String riskLevelSourceType) {

		this.riskLevelSourceType = riskLevelSourceType;
	}

	/**
	 * Gets the risk level source name.
	 *
	 * @return the riskLevelSourceName
	 */
	public String getRiskLevelSourceName() {

		return riskLevelSourceName;
	}

	/**
	 * Sets the risk level source name.
	 *
	 * @param riskLevelSourceName the riskLevelSourceName to set
	 */
	public void setRiskLevelSourceName(String riskLevelSourceName) {

		this.riskLevelSourceName = riskLevelSourceName;
	}

	/**
	 * Gets the risk score.
	 *
	 * @return the riskScore
	 */
	public Object getRiskScore() {

		return riskScore;
	}

	/**
	 * Sets the risk score.
	 *
	 * @param riskScore the riskScore to set
	 */
	public void setRiskScore(Object riskScore) {

		this.riskScore = riskScore;
	}

	/**
	 * Gets the risk value calculation rate.
	 *
	 * @return the riskValueCalculationRate
	 */
	public int getRiskValueCalculationRate() {

		return riskValueCalculationRate;
	}

	/**
	 * Sets the risk value calculation rate.
	 *
	 * @param riskValueCalculationRate the riskValueCalculationRate to set
	 */
	public void setRiskValueCalculationRate(int riskValueCalculationRate) {

		this.riskValueCalculationRate = riskValueCalculationRate;
	}

}
