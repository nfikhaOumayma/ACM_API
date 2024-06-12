/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 * {@link GenericModel} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@MappedSuperclass
public class GenericModel implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3974801554088107052L;

	/** The enabled. */
	@Column(name = "ACM_ENABLED", nullable = false)
	private Boolean enabled;

	/** The date insertion. */
	@Column(name = "DATE_INSERTION")
	private Date dateInsertion;

	/** The date last update. */
	@Column(name = "DATE_LAST_UPDATE")
	private Date dateLastUpdate;

	/** The acm version. */
	@Column(name = "ACM_VERSION")
	private Integer acmVersion;

	/** The updated by. */
	@Column(name = "UPDATED_BY", length = 256)
	private String updatedBy;

	/** The insert by. */
	@Column(name = "INSERT_BY", length = 256)
	private String insertBy;

	/**
	 * Instantiates a new generic model.
	 */
	public GenericModel() {

		/*
		 * Empty
		 */
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

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the date last update.
	 *
	 * @return the date last update
	 */
	public Date getDateLastUpdate() {

		return dateLastUpdate;
	}

	/**
	 * Sets the date last update.
	 *
	 * @param dateLastUpdate the new date last update
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {

		this.dateLastUpdate = dateLastUpdate;
	}

	/**
	 * Gets the acm version.
	 *
	 * @return the acm version
	 */
	public Integer getAcmVersion() {

		return acmVersion;
	}

	/**
	 * Sets the acm version.
	 *
	 * @param acmVersion the new acm version
	 */
	public void setAcmVersion(Integer acmVersion) {

		this.acmVersion = acmVersion;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updated by
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the new updated by
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "GenericModel [ENABLED=" + enabled + ", DATEINSERTION=" + dateInsertion
				+ ", DATELASTUPDATE=" + dateLastUpdate + ", ACMVERSION=" + acmVersion
				+ ", UPDATEDBY=" + updatedBy + ", INSERTBY=" + insertBy + "]";
	}

}
