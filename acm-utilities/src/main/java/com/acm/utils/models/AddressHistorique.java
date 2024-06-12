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
 * {@link AddressHistorique} class.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
@Entity
@Table(name = "ACM_ADDRESS_HISTORIQUE")
public class AddressHistorique extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8144301195251779830L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_ADDRESS_HISTORIQUE", unique = true, nullable = false)
	private Long id;

	/** The id address ACM. */
	@Column(name = "ID_ACM_ADDRESS")
	private Long idAddressACM;

	/** The old address. */
	@Column(name = "OLD_ADDRESS")
	private String oldAddress;

	/** The new address. */
	@Column(name = "NEW_ADDRESS")
	private String newAddress;

	/** The reason update. */
	@Column(name = "REASONUPDATE")
	private String reasonUpdate;

	/**
	 * Instantiates a new address historique.
	 */
	public AddressHistorique() {

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id address ACM.
	 *
	 * @return the id address ACM
	 */
	public Long getIdAddressACM() {

		return idAddressACM;
	}

	/**
	 * Sets the id address ACM.
	 *
	 * @param idAddressACM the new id address ACM
	 */
	public void setIdAddressACM(Long idAddressACM) {

		this.idAddressACM = idAddressACM;
	}

	/**
	 * Gets the old address.
	 *
	 * @return the old address
	 */
	public String getOldAddress() {

		return oldAddress;
	}

	/**
	 * Sets the old address.
	 *
	 * @param oldAddress the new old address
	 */
	public void setOldAddress(String oldAddress) {

		this.oldAddress = oldAddress;
	}

	/**
	 * Gets the new address.
	 *
	 * @return the new address
	 */
	public String getNewAddress() {

		return newAddress;
	}

	/**
	 * Gets the reason update.
	 *
	 * @return the reasonUpdate
	 */
	public String getReasonUpdate() {

		return reasonUpdate;
	}

	/**
	 * Sets the reason update.
	 *
	 * @param reasonUpdate the reasonUpdate to set
	 */
	public void setReasonUpdate(String reasonUpdate) {

		this.reasonUpdate = reasonUpdate;
	}

	/**
	 * Sets the new address.
	 *
	 * @param newAddress the new new address
	 */
	public void setNewAddress(String newAddress) {

		this.newAddress = newAddress;
	}

}
