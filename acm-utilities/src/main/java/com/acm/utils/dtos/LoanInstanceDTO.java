/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link LoanInstanceDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class LoanInstanceDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8798597804826599264L;

	/** The id. */
	private Long id;

	/** The id loan. */
	@Mapping("loan.idLoan")
	private Long idLoan;

	/** The code. */
	private Integer code;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The code statut loan. */
	private Long codeStatutLoan;

	/** The statut loan. */
	private String statutLoan;

	/** The statut loan. */
	private String ihmRoot;

	/** The client. */
	private String client;

	/** The process name. */
	private String processName;

	/** The orderEtapeProcess. */
	private Integer orderEtapeProcess;

	/** The showIb. */
	private Boolean showIb;

	/** The codeStautIb. */
	private Integer codeStautIb;

	/** The action user. */
	private String actionUser;

	/** The enabled. */
	private Boolean enabled;

	/** The journal entry. */
	private String journalEntry;

	/** The ib ihm root. */
	private String ibIhmRoot;

	/**
	 * Instantiates a new loan instance DTO.
	 */
	public LoanInstanceDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan instance DTO.
	 *
	 * @param idLoan the id loan
	 * @param code the code
	 * @param libelle the libelle
	 * @param description the description
	 * @param codeStatutLoan the code statut loan
	 * @param statutLoan the statut loan
	 * @param ihmRoot the ihm root
	 * @param client the client
	 * @param processName the process name
	 * @param orderEtapeProcess the order etape process
	 * @param showIb the show ib
	 * @param codeStautIb the code staut ib
	 * @param enabled the enabled
	 * @param ibIhmRoot the ib ihm root
	 */
	public LoanInstanceDTO(Long idLoan, Integer code, String libelle, String description,
			Long codeStatutLoan, String statutLoan, String ihmRoot, String client,
			String processName, Integer orderEtapeProcess, Boolean showIb, Integer codeStautIb,
			Boolean enabled, String ibIhmRoot) {

		this.idLoan = idLoan;
		this.code = code;
		this.libelle = libelle;
		this.description = description;
		this.codeStatutLoan = codeStatutLoan;
		this.statutLoan = statutLoan;
		this.ihmRoot = ihmRoot;
		this.client = client;
		this.processName = processName;
		this.orderEtapeProcess = orderEtapeProcess;
		this.showIb = showIb;
		this.codeStautIb = codeStautIb;
		this.enabled = enabled;
		this.ibIhmRoot = ibIhmRoot;
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
	 * Gets the code statut loan.
	 *
	 * @return the codeStatutLoan
	 */
	public Long getCodeStatutLoan() {

		return codeStatutLoan;
	}

	/**
	 * Sets the code statut loan.
	 *
	 * @param codeStatutLoan the codeStatutLoan to set
	 */
	public void setCodeStatutLoan(Long codeStatutLoan) {

		this.codeStatutLoan = codeStatutLoan;
	}

	/**
	 * Gets the statut loan.
	 *
	 * @return the statutLoan
	 */
	public String getStatutLoan() {

		return statutLoan;
	}

	/**
	 * Sets the statut loan.
	 *
	 * @param statutLoan the statutLoan to set
	 */
	public void setStatutLoan(String statutLoan) {

		this.statutLoan = statutLoan;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public Integer getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(Integer code) {

		this.code = code;
	}

	/**
	 * Gets the libelle.
	 *
	 * @return the libelle
	 */
	public String getLibelle() {

		return libelle;
	}

	/**
	 * Sets the libelle.
	 *
	 * @param libelle the libelle to set
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
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
	 * Gets the ihm root.
	 *
	 * @return the ihmRoot
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihm root.
	 *
	 * @param ihmRoot the ihmRoot to set
	 */
	public void setIhmRoot(String ihmRoot) {

		this.ihmRoot = ihmRoot;
	}

	/**
	 * Gets the client.
	 *
	 * @return the client
	 */
	public String getClient() {

		return client;
	}

	/**
	 * Sets the client.
	 *
	 * @param client the client to set
	 */
	public void setClient(String client) {

		this.client = client;
	}

	/**
	 * Gets the process name.
	 *
	 * @return the processName
	 */
	public String getProcessName() {

		return processName;
	}

	/**
	 * Sets the process name.
	 *
	 * @param processName the processName to set
	 */
	public void setProcessName(String processName) {

		this.processName = processName;
	}

	/**
	 * Gets the order etape process.
	 *
	 * @return the orderEtapeProcess
	 */
	public Integer getOrderEtapeProcess() {

		return orderEtapeProcess;
	}

	/**
	 * Sets the order etape process.
	 *
	 * @param orderEtapeProcess the orderEtapeProcess to set
	 */
	public void setOrderEtapeProcess(Integer orderEtapeProcess) {

		this.orderEtapeProcess = orderEtapeProcess;
	}

	/**
	 * Gets the show ib.
	 *
	 * @return the showIb
	 */
	public Boolean getShowIb() {

		return showIb;
	}

	/**
	 * Sets the show ib.
	 *
	 * @param showIb the showIb to set
	 */
	public void setShowIb(Boolean showIb) {

		this.showIb = showIb;
	}

	/**
	 * Gets the code staut ib.
	 *
	 * @return the codeStautIb
	 */
	public Integer getCodeStautIb() {

		return codeStautIb;
	}

	/**
	 * Sets the code staut ib.
	 *
	 * @param codeStautIb the codeStautIb to set
	 */
	public void setCodeStautIb(Integer codeStautIb) {

		this.codeStautIb = codeStautIb;
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
	 * Gets the action user.
	 *
	 * @return the actionUser
	 */
	public String getActionUser() {

		return actionUser;
	}

	/**
	 * Sets the action user.
	 *
	 * @param actionUser the actionUser to set
	 */
	public void setActionUser(String actionUser) {

		this.actionUser = actionUser;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanInstanceDTO [id=" + id + ", idLoan=" + idLoan + ", code=" + code + ", libelle="
				+ libelle + ", description=" + description + ", codeStatutLoan=" + codeStatutLoan
				+ ", statutLoan=" + statutLoan + ", ihmRoot=" + ihmRoot + ", client=" + client
				+ ", processName=" + processName + ", orderEtapeProcess=" + orderEtapeProcess
				+ ", showIb=" + showIb + ", codeStautIb=" + codeStautIb + ", enabled=" + enabled
				+ "]";
	}

	/**
	 * Gets the journal entry.
	 *
	 * @return the journal entry
	 */
	public String getJournalEntry() {

		return journalEntry;
	}

	/**
	 * Sets the journal entry.
	 *
	 * @param journalEntry the new journal entry
	 */
	public void setJournalEntry(String journalEntry) {

		this.journalEntry = journalEntry;
	}

	/**
	 * Gets the ib ihm root.
	 *
	 * @return the ib ihm root
	 */
	public String getIbIhmRoot() {

		return ibIhmRoot;
	}

	/**
	 * Sets the ib ihm root.
	 *
	 * @param ibIhmRoot the new ib ihm root
	 */
	public void setIbIhmRoot(String ibIhmRoot) {

		this.ibIhmRoot = ibIhmRoot;
	}

}
