/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link LoanParticipants} class.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
@Entity
@Table(name = "ACM_LOAN_PARTICIPANTS")
public class LoanParticipants extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1346991307845741519L;

	/** The id document. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LOAN_PARTICIPANTS", unique = true, nullable = false)
	private Long id;

	/** The loan. */
	@Column(name = "ID_ACM_LOAN")
	private Long idLoan;

	/** The user. */
	@Column(name = "USERNAME")
	private String username;

	/** The date debut. */
	@Column(name = "DATE_DEBUT")
	private Date dateDebut;

	/** The date fin. */
	@Column(name = "DATE_FIN")
	private Date dateFin;

	/**
	 * Instantiates a new loan participants.
	 */
	public LoanParticipants() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan participants (USED IN SAVE METHOD).
	 *
	 * @param idLoan the id loan
	 * @param username the username
	 */
	public LoanParticipants(Long idLoan, String username) {

		this.idLoan = idLoan;
		this.username = username;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanParticipants [" + (id != null ? "id=" + id + ", " : "")
				+ (idLoan != null ? "idLoan=" + idLoan + ", " : "")
				+ (username != null ? "username=" + username + ", " : "")
				+ (dateDebut != null ? "dateDebut=" + dateDebut + ", " : "")
				+ (dateFin != null ? "dateFin=" + dateFin : "") + "]";
	}
}
