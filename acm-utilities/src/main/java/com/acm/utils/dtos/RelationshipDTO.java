/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link RelationshipDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class RelationshipDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2006585871476496607L;

	/** The relationship ID. */
	private Long relationshipID;

	/** The name. */
	private String name;

	/** The inverse name. */
	private String inverseName;

	/** The directional. */
	private Integer directional;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new product loan reasons DTO.
	 */
	public RelationshipDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new relationship DTO.
	 *
	 * @param relationshipID the relationship ID
	 * @param name the name
	 * @param inverseName the inverse name
	 * @param directional the directional
	 * @param enabled the enabled
	 */
	public RelationshipDTO(Long relationshipID, String name, String inverseName,
			Integer directional, Boolean enabled) {

		this.relationshipID = relationshipID;
		this.name = name;
		this.inverseName = inverseName;
		this.directional = directional;
		this.enabled = enabled;
	}

	/**
	 * Gets the relationship ID.
	 *
	 * @return the relationshipID
	 */
	public Long getRelationshipID() {

		return relationshipID;
	}

	/**
	 * Sets the relationship ID.
	 *
	 * @param relationshipID the relationshipID to set
	 */
	public void setRelationshipID(Long relationshipID) {

		this.relationshipID = relationshipID;
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
	 * Gets the inverse name.
	 *
	 * @return the inverseName
	 */
	public String getInverseName() {

		return inverseName;
	}

	/**
	 * Sets the inverse name.
	 *
	 * @param inverseName the inverseName to set
	 */
	public void setInverseName(String inverseName) {

		this.inverseName = inverseName;
	}

	/**
	 * Gets the directional.
	 *
	 * @return the directional
	 */
	public Integer getDirectional() {

		return directional;
	}

	/**
	 * Sets the directional.
	 *
	 * @param directional the directional to set
	 */
	public void setDirectional(Integer directional) {

		this.directional = directional;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RelationshipDTO [relationshipID=" + relationshipID + ", name=" + name
				+ ", inverseName=" + inverseName + ", directional=" + directional + ", enabled="
				+ enabled + "]";
	}

}
