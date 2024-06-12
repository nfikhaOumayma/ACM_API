/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link AddressHistoriqueDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
public class AddressHistoriqueDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7266694975440466359L;

	/** The id. */
	private Long id;

	/** The id address ACM. */
	private Long idAddressACM;

	/** The old address. */
	private AddressDTO oldAddressDTO;

	/** The new address. */
	private AddressDTO newAddressDTO;

	/** The date insertion. */
	private Date dateInsertion;

	/** The insert by. */
	private String insertBy;

	/** The reason update. */
	private String reasonUpdate;

	/**
	 * Instantiates a new address historique DTO.
	 */
	public AddressHistoriqueDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new address historique DTO.
	 *
	 * @param idAddressACM the id address ACM
	 * @param oldAddressDTO the old address DTO
	 * @param newAddressDTO the new address DTO
	 * @param reasonUpdate the reason update
	 */
	public AddressHistoriqueDTO(Long idAddressACM, AddressDTO oldAddressDTO,
			AddressDTO newAddressDTO, String reasonUpdate) {

		this.idAddressACM = idAddressACM;
		this.oldAddressDTO = oldAddressDTO;
		this.newAddressDTO = newAddressDTO;
		this.reasonUpdate = reasonUpdate;
	}

	/**
	 * Instantiates a new address historique DTO.
	 *
	 * @param id the id
	 * @param idAddressACM the id address ACM
	 * @param oldAddressDTO the old address DTO
	 * @param newAddressDTO the new address DTO
	 * @param dateInsertion the date insertion
	 * @param insertBy the insert by
	 * @param reasonUpdate the reason update
	 */
	public AddressHistoriqueDTO(Long id, Long idAddressACM, AddressDTO oldAddressDTO,
			AddressDTO newAddressDTO, Date dateInsertion, String insertBy, String reasonUpdate) {

		this.id = id;
		this.idAddressACM = idAddressACM;
		this.oldAddressDTO = oldAddressDTO;
		this.newAddressDTO = newAddressDTO;
		this.dateInsertion = dateInsertion;
		this.insertBy = insertBy;
		this.reasonUpdate = reasonUpdate;
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
	 * Gets the old address DTO.
	 *
	 * @return the oldAddressDTO
	 */
	public AddressDTO getOldAddressDTO() {

		return oldAddressDTO;
	}

	/**
	 * Sets the old address DTO.
	 *
	 * @param oldAddressDTO the oldAddressDTO to set
	 */
	public void setOldAddressDTO(AddressDTO oldAddressDTO) {

		this.oldAddressDTO = oldAddressDTO;
	}

	/**
	 * Gets the new address DTO.
	 *
	 * @return the newAddressDTO
	 */
	public AddressDTO getNewAddressDTO() {

		return newAddressDTO;
	}

	/**
	 * Sets the new address DTO.
	 *
	 * @param newAddressDTO the newAddressDTO to set
	 */
	public void setNewAddressDTO(AddressDTO newAddressDTO) {

		this.newAddressDTO = newAddressDTO;
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

}
