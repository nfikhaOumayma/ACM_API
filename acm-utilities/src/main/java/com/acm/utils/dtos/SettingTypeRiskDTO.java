/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * The Class SettingTypeRiskDTO.
 */
public class SettingTypeRiskDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	private Long id;

	/** The description. */
	private String label;

	/** The description. */
	private String description;

	/** The enabled. */
	private boolean enabled;

	/** The echelle type risks. */
	private List<EchelleTypeRiskDTO> echelleTypeRisks;

	/** The editable. */
	private boolean editable;

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
	 * Gets the label.
	 *
	 * @return the label
	 */
	public String getLabel() {

		return label;
	}

	/**
	 * Sets the label.
	 *
	 * @param label the label to set
	 */
	public void setLabel(String label) {

		this.label = label;
	}

	/**
	 * Checks if is enabled.
	 *
	 * @return the enabled
	 */
	public boolean isEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the echelle type risks.
	 *
	 * @return the echelle type risks
	 */
	public List<EchelleTypeRiskDTO> getEchelleTypeRisks() {

		return echelleTypeRisks;
	}

	/**
	 * Sets the echelle type risks.
	 *
	 * @param echelleTypeRisks the new echelle type risks
	 */
	public void setEchelleTypeRisks(List<EchelleTypeRiskDTO> echelleTypeRisks) {

		this.echelleTypeRisks = echelleTypeRisks;
	}

	/**
	 * Gets the editable.
	 *
	 * @return the editable
	 */
	public boolean getEditable() {

		return editable;
	}

	/**
	 * Sets the editable.
	 *
	 * @param editable the editable to set
	 */
	public void setEditable(boolean editable) {

		this.editable = editable;
	}

}
