/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingStatutWorkflowDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public class SettingStatutWorkflowDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6710785776856241984L;

	/** The id. */
	private Long id;

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

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The ihmRoot. */
	private String ihmRoot;

	/** The client. */
	private String client;

	/** The statut loan. */
	private String processName;

	/** The order. */
	private Integer orderEtapeProcess;

	/** The showIb. */
	private Boolean showIb;

	/** The codeStautIb. */
	private Integer codeStautIb;

	/** The enabled. */
	private Boolean enabled;

	/** The is new Loan. */
	private Boolean isNewLoan;

	/**
	 * Instantiates a new setting statut workflow DTO.
	 */
	public SettingStatutWorkflowDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting statut workflow DTO.
	 *
	 * @param loanDTO the loan DTO
	 * @param client the client
	 * @param processName the process name
	 */
	public SettingStatutWorkflowDTO(LoanDTO loanDTO, String client, String processName) {

		this.loanDTO = loanDTO;
		this.client = client;
		this.processName = processName;
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
	 * Gets the loan DTO.
	 *
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the ihmRoot.
	 *
	 * @return the ihmRoot
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihmRoot.
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
	 * Gets the checks if is new loan.
	 *
	 * @return the isNewLOan
	 */
	public Boolean getIsNewLoan() {

		return isNewLoan;
	}

	/**
	 * Sets the checks if is new loan.
	 *
	 * @param isNewLoan the isNewLoan to set
	 */
	public void setIsNewLoan(Boolean isNewLoan) {

		this.isNewLoan = isNewLoan;
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

		return "SettingStatutWorkflowDTO [id=" + id + ", code=" + code + ", libelle=" + libelle
				+ ", description=" + description + ", codeStatutLoan=" + codeStatutLoan
				+ ", statutLoan=" + statutLoan + ", loanDTO=" + loanDTO + ", ihmRoot=" + ihmRoot
				+ ", client=" + client + ", processName=" + processName + ", orderEtapeProcess="
				+ orderEtapeProcess + ", showIb=" + showIb + ", codeStautIb=" + codeStautIb
				+ ", enabled=" + enabled + ", isNewLoan=" + isNewLoan + "]";
	}

}
