/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link LoanInstance} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Entity
@Table(name = "ACM_LOAN_INSTANCE")
public class LoanInstance extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4100570609999190768L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_LOAN_INSTANCE", unique = true, nullable = false)
	private Long id;

	/** The loan. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_LOAN")
	private Loan loan;

	/** The code. */
	@Column(name = "ID_ACM_WORKFLOW_STEP", nullable = false)
	private Integer code;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The code statut loan. */
	@Column(name = "CODE_STATUT_LOAN", nullable = false)
	private Long codeStatutLoan;

	/** The statut loan. */
	@Column(name = "STATUT_LOAN")
	private String statutLoan;

	/** The statut loan. */
	@Column(name = "IHM_WEB_ROOT")
	private String ihmRoot;

	/** The client. */
	@Column(name = "CLIENT")
	private String client;

	/** The process name. */
	@Column(name = "BPMN_PROCESS_NAME")
	private String processName;

	/** The orderEtapeProcess. */
	@Column(name = "ORDER_ETAPE_PROCESS")
	private Integer orderEtapeProcess;

	/** The showIb. */
	@Column(name = "SHOW_IB")
	private Boolean showIb;

	/** The codeStautIb. */
	@Column(name = "CODE_STATUT_IB")
	private Integer codeStautIb;

	/** The action user. */
	@Column(name = "ACTION_USER")
	private String actionUser;

	/** The journal entry. */
	@Column(name = "JOURNAL_ENTRY")
	private String journalEntry;

	/** The loan instance groups. */
	@OneToMany(mappedBy = "loanInstance", cascade = CascadeType.ALL)
	private List<AcmLoanInstanceAcmGroupeApproval> approvers = new ArrayList<>();

	/** The ib ihm root. */
	@Column(name = "IB_IHM_ROOT")
	private String ibIhmRoot;

	/** The Charge fees. */
	@OneToMany(mappedBy = "loanInstance")
	private Set<ChargeFees> ChargeFees = new HashSet<>();

	/**
	 * Instantiates a new loan instance.
	 */
	public LoanInstance() {

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
	 * Gets the loan.
	 *
	 * @return the loan
	 */
	public Loan getLoan() {

		return loan;
	}

	/**
	 * Sets the loan.
	 *
	 * @param loan the loan to set
	 */
	public void setLoan(Loan loan) {

		this.loan = loan;
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

	/**
	 * Gets the approvers.
	 *
	 * @return the approvers
	 */
	public List<AcmLoanInstanceAcmGroupeApproval> getApprovers() {

		return approvers;
	}

	/**
	 * Sets the approvers.
	 *
	 * @param approvers the new approvers
	 */
	public void setApprovers(List<AcmLoanInstanceAcmGroupeApproval> approvers) {

		this.approvers = approvers;
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

	/**
	 * Gets the charge fees.
	 *
	 * @return the charge fees
	 */
	public Set<ChargeFees> getChargeFees() {

		return ChargeFees;
	}

	/**
	 * Sets the charge fees.
	 *
	 * @param chargeFees the new charge fees
	 */
	public void setChargeFees(Set<ChargeFees> chargeFees) {

		ChargeFees = chargeFees;
	}

}
