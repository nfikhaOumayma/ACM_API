/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

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
 * {@link EchelleTypeRisk} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_RISQUE_TYPE_ECHELLE")
public class EchelleTypeRisk implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_TYPE_ECHELLE", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "LABEL", nullable = false)
	private String label;
	/** The code. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The setting type risk. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_TYPE_RISQUE")
	private SettingTypeRisk settingTypeRisk;

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
	 * @param label the new label
	 */
	public void setLabel(String label) {

		this.label = label;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the setting type risk.
	 *
	 * @return the setting type risk
	 */
	public SettingTypeRisk getSettingTypeRisk() {

		return settingTypeRisk;
	}

	/**
	 * Sets the setting type risk.
	 *
	 * @param settingTypeRisk the new setting type risk
	 */
	public void setSettingTypeRisk(SettingTypeRisk settingTypeRisk) {

		this.settingTypeRisk = settingTypeRisk;
	}

}
