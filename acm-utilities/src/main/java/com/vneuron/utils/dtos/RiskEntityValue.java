/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

import java.util.ArrayList;

/**
 * {@link RiskEntityValue } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class RiskEntityValue {

	/** The id. */
	public int id;

	/** The name. */
	public String name;

	/** The risk value type. */
	public String risk_value_type;

	/** The value. */
	public String value;

	/** The start. */
	public Object start;

	/** The end. */
	public Object end;

	/** The risk level id. */
	public int riskLevelId;

	/** The risk entity id. */
	public int riskEntityId;

	/** The risk entity category id. */
	public Object riskEntityCategoryId;

	/** The risk entity category name. */
	public Object riskEntityCategoryName;

	/** The risk value condition items. */
	public ArrayList<Object> riskValueConditionItems;

	/** The conditions text representation. */
	public String conditionsTextRepresentation;

	/** The risk score. */
	public double riskScore;

	/** The risk name. */
	public String riskName;

	/** The has risk score. */
	public boolean hasRiskScore;

	/** The risk formula. */
	public String riskFormula;

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
	 * Gets the risk value type.
	 *
	 * @return the risk_value_type
	 */
	public String getRisk_value_type() {

		return risk_value_type;
	}

	/**
	 * Sets the risk value type.
	 *
	 * @param risk_value_type the risk_value_type to set
	 */
	public void setRisk_value_type(String risk_value_type) {

		this.risk_value_type = risk_value_type;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(String value) {

		this.value = value;
	}

	/**
	 * Gets the start.
	 *
	 * @return the start
	 */
	public Object getStart() {

		return start;
	}

	/**
	 * Sets the start.
	 *
	 * @param start the start to set
	 */
	public void setStart(Object start) {

		this.start = start;
	}

	/**
	 * Gets the end.
	 *
	 * @return the end
	 */
	public Object getEnd() {

		return end;
	}

	/**
	 * Sets the end.
	 *
	 * @param end the end to set
	 */
	public void setEnd(Object end) {

		this.end = end;
	}

	/**
	 * Gets the risk level id.
	 *
	 * @return the riskLevelId
	 */
	public int getRiskLevelId() {

		return riskLevelId;
	}

	/**
	 * Sets the risk level id.
	 *
	 * @param riskLevelId the riskLevelId to set
	 */
	public void setRiskLevelId(int riskLevelId) {

		this.riskLevelId = riskLevelId;
	}

	/**
	 * Gets the risk entity id.
	 *
	 * @return the riskEntityId
	 */
	public int getRiskEntityId() {

		return riskEntityId;
	}

	/**
	 * Sets the risk entity id.
	 *
	 * @param riskEntityId the riskEntityId to set
	 */
	public void setRiskEntityId(int riskEntityId) {

		this.riskEntityId = riskEntityId;
	}

	/**
	 * Gets the risk entity category id.
	 *
	 * @return the riskEntityCategoryId
	 */
	public Object getRiskEntityCategoryId() {

		return riskEntityCategoryId;
	}

	/**
	 * Sets the risk entity category id.
	 *
	 * @param riskEntityCategoryId the riskEntityCategoryId to set
	 */
	public void setRiskEntityCategoryId(Object riskEntityCategoryId) {

		this.riskEntityCategoryId = riskEntityCategoryId;
	}

	/**
	 * Gets the risk entity category name.
	 *
	 * @return the riskEntityCategoryName
	 */
	public Object getRiskEntityCategoryName() {

		return riskEntityCategoryName;
	}

	/**
	 * Sets the risk entity category name.
	 *
	 * @param riskEntityCategoryName the riskEntityCategoryName to set
	 */
	public void setRiskEntityCategoryName(Object riskEntityCategoryName) {

		this.riskEntityCategoryName = riskEntityCategoryName;
	}

	/**
	 * Gets the risk value condition items.
	 *
	 * @return the riskValueConditionItems
	 */
	public ArrayList<Object> getRiskValueConditionItems() {

		return riskValueConditionItems;
	}

	/**
	 * Sets the risk value condition items.
	 *
	 * @param riskValueConditionItems the riskValueConditionItems to set
	 */
	public void setRiskValueConditionItems(ArrayList<Object> riskValueConditionItems) {

		this.riskValueConditionItems = riskValueConditionItems;
	}

	/**
	 * Gets the conditions text representation.
	 *
	 * @return the conditionsTextRepresentation
	 */
	public String getConditionsTextRepresentation() {

		return conditionsTextRepresentation;
	}

	/**
	 * Sets the conditions text representation.
	 *
	 * @param conditionsTextRepresentation the conditionsTextRepresentation to set
	 */
	public void setConditionsTextRepresentation(String conditionsTextRepresentation) {

		this.conditionsTextRepresentation = conditionsTextRepresentation;
	}

	/**
	 * Gets the risk score.
	 *
	 * @return the riskScore
	 */
	public double getRiskScore() {

		return riskScore;
	}

	/**
	 * Sets the risk score.
	 *
	 * @param riskScore the riskScore to set
	 */
	public void setRiskScore(double riskScore) {

		this.riskScore = riskScore;
	}

	/**
	 * Gets the risk name.
	 *
	 * @return the riskName
	 */
	public String getRiskName() {

		return riskName;
	}

	/**
	 * Sets the risk name.
	 *
	 * @param riskName the riskName to set
	 */
	public void setRiskName(String riskName) {

		this.riskName = riskName;
	}

	/**
	 * Checks if is checks for risk score.
	 *
	 * @return the hasRiskScore
	 */
	public boolean isHasRiskScore() {

		return hasRiskScore;
	}

	/**
	 * Sets the checks for risk score.
	 *
	 * @param hasRiskScore the hasRiskScore to set
	 */
	public void setHasRiskScore(boolean hasRiskScore) {

		this.hasRiskScore = hasRiskScore;
	}

	/**
	 * Gets the risk formula.
	 *
	 * @return the riskFormula
	 */
	public String getRiskFormula() {

		return riskFormula;
	}

	/**
	 * Sets the risk formula.
	 *
	 * @param riskFormula the riskFormula to set
	 */
	public void setRiskFormula(String riskFormula) {

		this.riskFormula = riskFormula;
	}

}
