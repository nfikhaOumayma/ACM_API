/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanParticipantsDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
public class LoanParticipantsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4995417642207776407L;

	/** The id document. */
	private Long id;

	/** The id loan. */
	private Long idLoan;

	/** The user. */
	private String username;

	/** The date debut. */
	private Date dateDebut;

	/** The date fin. */
	private Date dateFin;

	/**
	 * Instantiates a new loan participants DTO.
	 */
	public LoanParticipantsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan participants DTO.
	 *
	 * @param idLoan the id loan
	 * @param username the username
	 */
	public LoanParticipantsDTO(Long idLoan, String username) {

		this.idLoan = idLoan;
		this.username = username;
	}

	/**
	 * Instantiates a new loan participants DTO.
	 *
	 * @param id the id
	 * @param idLoan the id loan
	 * @param username the username
	 * @param dateDebut the date debut
	 * @param dateFin the date fin
	 */
	public LoanParticipantsDTO(Long id, Long idLoan, String username, Date dateDebut,
			Date dateFin) {

		this.id = id;
		this.idLoan = idLoan;
		this.username = username;
		this.dateDebut = dateDebut;
		this.dateFin = dateFin;
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id loan.
	 *
	 * @return the idLoan
	 */
	public Long getIdLoan() {

		return idLoan;
	}

	/**
	 * Sets the id loan.
	 *
	 * @param idLoan the idLoan to set
	 */
	public void setIdLoan(Long idLoan) {

		this.idLoan = idLoan;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the username to set
	 */
	public void setUsername(String username) {

		this.username = username;
	}

	/**
	 * Gets the date debut.
	 *
	 * @return the dateDebut
	 */
	public Date getDateDebut() {

		return dateDebut;
	}

	/**
	 * Sets the date debut.
	 *
	 * @param dateDebut the dateDebut to set
	 */
	public void setDateDebut(Date dateDebut) {

		this.dateDebut = dateDebut;
	}

	/**
	 * Gets the date fin.
	 *
	 * @return the dateFin
	 */
	public Date getDateFin() {

		return dateFin;
	}

	/**
	 * Sets the date fin.
	 *
	 * @param dateFin the dateFin to set
	 */
	public void setDateFin(Date dateFin) {

		this.dateFin = dateFin;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanParticipantsDTO [" + (id != null ? "id=" + id + ", " : "")
				+ (idLoan != null ? "idLoan=" + idLoan + ", " : "")
				+ (username != null ? "username=" + username + ", " : "")
				+ (dateDebut != null ? "dateDebut=" + dateDebut + ", " : "")
				+ (dateFin != null ? "dateFin=" + dateFin : "") + "]";
	}

}
