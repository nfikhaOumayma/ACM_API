/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link SettingTypeRisk} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_SETTING_TYPE_RISQUE")

public class SettingTypeRisk extends GenericModel implements Serializable {
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_TYPE_RISQUE", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "LABEL", nullable = false)
	private String label;
	/** The code. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The echelle type risks. */
	@OneToMany(mappedBy = "settingTypeRisk", cascade = CascadeType.ALL)
	private List<EchelleTypeRisk> echelleTypeRisks = new ArrayList<EchelleTypeRisk>();

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
	 * Gets the echelle type risks.
	 *
	 * @return the echelle type risks
	 */
	public List<EchelleTypeRisk> getEchelleTypeRisks() {

		return echelleTypeRisks;
	}

	/**
	 * Sets the echelle type risks.
	 *
	 * @param echelleTypeRisks the new echelle type risks
	 */
	public void setEchelleTypeRisks(List<EchelleTypeRisk> echelleTypeRisks) {

		this.echelleTypeRisks = echelleTypeRisks;
	}

}
