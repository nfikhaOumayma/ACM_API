/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.Mapping;

/**
 * The Class AcmCollectionDTO.
 */
public class AcmCollectionDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8594481537779313132L;

	/** The id. */
	private Long id;

	/** The type customer. */
	private String typeCustomer;

	/** The customer id extern. */
	private Long customerIdExtern;

	/** The id loan extern. */
	private Long idLoanExtern;

	/** The account number. */
	private String accountNumber;

	/** The product description. */
	private String productDescription;

	/** The customer name. */
	private String customerName;

	/** The branch description. */
	private String branchDescription;

	/** The amount. */
	private BigDecimal amount;

	/** The loan officer. */
	private String loanOfficer;

	/** The first unpaid installment. */
	private Date firstUnpaidInstallment;

	/** The unpaid amount. */
	private BigDecimal unpaidAmount;

	/** The late days. */
	private Integer lateDays;

	/** The number of unpaid installment. */
	private Integer numberOfUnpaidInstallment;

	/** The status. */
	private Integer status;

	/** The pending action. */
	private Long idAcmCollectionStep;

	/** The action. */
	private String action;

	/** The enabled. */
	private Boolean enabled;

	/** The date insertion. */
	private Date dateInsertion;

	/** The insert by. */
	private String insertBy;

	/** The product id. */
	private Long productId;

	/** The branch id. */
	private Long branchId;

	/** The currency decimal places. */
	private Integer currencyDecimalPlaces;

	/** The currency symbol. */
	private String currencySymbol;

	/** The date last update. */
	private Date dateLastUpdate;

	/** The collection instances dtos. */
	@Mapping("collectionInstances")
	private List<CollectionInstanceDTO> collectionInstancesDtos = new ArrayList<>();

	/** The pending action changed. */
	private Boolean pendingActionChanged;

	/** The available date. */
	private Date availableDate;

	/** The owner. */
	private String owner;

	/** The owner name. */
	private String ownerName;

	/** The group owner. */
	private String groupOwner;

	/** The group owner name. */
	private String groupOwnerName;

	/** The type. */
	private String collectionType;

	/** The id parent collection. */
	private Long idParentCollection;

	/** The Current status Label. */
	private String statutLibelle;

	/** The Done status Label. */
	private String statutLibelleDone;
	
	/** The statut workflow. */
	private String statutWorkflow;

	/**
	 * Gets the statut workflow.
	 *
	 * @return the statut workflow
	 */
	public String getStatutWorkflow() {
		return statutWorkflow;
	}

	/**
	 * Sets the statut workflow.
	 *
	 * @param statutWorkflow the new statut workflow
	 */
	public void setStatutWorkflow(String statutWorkflow) {
		this.statutWorkflow = statutWorkflow;
	}

	/**
	 * Instantiates a new acm collection DTO.
	 */

	public AcmCollectionDTO() {

		/*
		 * 
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
	 * Gets the type customer.
	 *
	 * @return the type customer
	 */
	public String getTypeCustomer() {

		return typeCustomer;
	}

	/**
	 * Sets the type customer.
	 *
	 * @param typeCustomer the new type customer
	 */
	public void setTypeCustomer(String typeCustomer) {

		this.typeCustomer = typeCustomer;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the account number
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the new account number
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
	}

	/**
	 * Gets the product description.
	 *
	 * @return the product description
	 */
	public String getProductDescription() {

		return productDescription;
	}

	/**
	 * Sets the product description.
	 *
	 * @param productDescription the new product description
	 */
	public void setProductDescription(String productDescription) {

		this.productDescription = productDescription;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customer name
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the new customer name
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the branch description.
	 *
	 * @return the branch description
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 *
	 * @param branchDescription the new branch description
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the loan officer.
	 *
	 * @return the loan officer
	 */
	public String getLoanOfficer() {

		return loanOfficer;
	}

	/**
	 * Sets the loan officer.
	 *
	 * @param loanOfficer the new loan officer
	 */
	public void setLoanOfficer(String loanOfficer) {

		this.loanOfficer = loanOfficer;
	}

	/**
	 * Gets the first unpaid installment.
	 *
	 * @return the first unpaid installment
	 */
	public Date getFirstUnpaidInstallment() {

		return firstUnpaidInstallment;
	}

	/**
	 * Sets the first unpaid installment.
	 *
	 * @param firstUnpaidInstallment the new first unpaid installment
	 */
	public void setFirstUnpaidInstallment(Date firstUnpaidInstallment) {

		this.firstUnpaidInstallment = firstUnpaidInstallment;
	}

	/**
	 * Gets the unpaid amount.
	 *
	 * @return the unpaid amount
	 */
	public BigDecimal getUnpaidAmount() {

		return unpaidAmount;
	}

	/**
	 * Sets the unpaid amount.
	 *
	 * @param unpaidAmount the new unpaid amount
	 */
	public void setUnpaidAmount(BigDecimal unpaidAmount) {

		this.unpaidAmount = unpaidAmount;
	}

	/**
	 * Gets the late days.
	 *
	 * @return the late days
	 */
	public Integer getLateDays() {

		return lateDays;
	}

	/**
	 * Sets the late days.
	 *
	 * @param lateDays the new late days
	 */
	public void setLateDays(Integer lateDays) {

		this.lateDays = lateDays;
	}

	/**
	 * Gets the number of unpaid installment.
	 *
	 * @return the number of unpaid installment
	 */
	public Integer getNumberOfUnpaidInstallment() {

		return numberOfUnpaidInstallment;
	}

	/**
	 * Sets the number of unpaid installment.
	 *
	 * @param numberOfUnpaidInstallment the new number of unpaid installment
	 */
	public void setNumberOfUnpaidInstallment(Integer numberOfUnpaidInstallment) {

		this.numberOfUnpaidInstallment = numberOfUnpaidInstallment;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public Integer getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the new status
	 */
	public void setStatus(Integer status) {

		this.status = status;
	}

	/**
	 * Gets the pending action.
	 *
	 * @return the pending action
	 */
	public Long getIdAcmCollectionStep() {

		return idAcmCollectionStep;
	}

	/**
	 * Sets the id acm collection step.
	 *
	 * @param idAcmCollectionStep the new id acm collection step
	 */
	public void setIdAcmCollectionStep(Long idAcmCollectionStep) {

		this.idAcmCollectionStep = idAcmCollectionStep;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the new action
	 */
	public void setAction(String action) {

		this.action = action;
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
	 * @param enabled the new enabled
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
	 * Gets the product id.
	 *
	 * @return the product id
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the new product id
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the branch id.
	 *
	 * @return the branch id
	 */
	public Long getBranchId() {

		return branchId;
	}

	/**
	 * Sets the branch id.
	 *
	 * @param branchId the new branch id
	 */
	public void setBranchId(Long branchId) {

		this.branchId = branchId;
	}

	/**
	 * Gets the currency decimal places.
	 *
	 * @return the currency decimal places
	 */
	public Integer getCurrencyDecimalPlaces() {

		return currencyDecimalPlaces;
	}

	/**
	 * Sets the currency decimal places.
	 *
	 * @param currencyDecimalPlaces the new currency decimal places
	 */
	public void setCurrencyDecimalPlaces(Integer currencyDecimalPlaces) {

		this.currencyDecimalPlaces = currencyDecimalPlaces;
	}

	/**
	 * Gets the currency symbol.
	 *
	 * @return the currency symbol
	 */
	public String getCurrencySymbol() {

		return currencySymbol;
	}

	/**
	 * Sets the currency symbol.
	 *
	 * @param currencySymbol the new currency symbol
	 */
	public void setCurrencySymbol(String currencySymbol) {

		this.currencySymbol = currencySymbol;
	}

	/**
	 * Gets the customer id extern.
	 *
	 * @return the customer id extern
	 */
	public Long getCustomerIdExtern() {

		return customerIdExtern;
	}

	/**
	 * Sets the customer id extern.
	 *
	 * @param customerIdExtern the new customer id extern
	 */
	public void setCustomerIdExtern(Long customerIdExtern) {

		this.customerIdExtern = customerIdExtern;
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
	 * Gets the id loan extern.
	 *
	 * @return the id loan extern
	 */
	public Long getIdLoanExtern() {

		return idLoanExtern;
	}

	/**
	 * Sets the id loan extern.
	 *
	 * @param idLoanExtern the new id loan extern
	 */
	public void setIdLoanExtern(Long idLoanExtern) {

		this.idLoanExtern = idLoanExtern;
	}

	/**
	 * Gets the collection instances dtos.
	 *
	 * @return the collection instances dtos
	 */
	public List<CollectionInstanceDTO> getCollectionInstancesDtos() {

		return collectionInstancesDtos;
	}

	/**
	 * Sets the collection instances dtos.
	 *
	 * @param collectionInstancesDtos the new collection instances dtos
	 */
	public void setCollectionInstancesDtos(List<CollectionInstanceDTO> collectionInstancesDtos) {

		this.collectionInstancesDtos = collectionInstancesDtos;
	}

	/**
	 * Gets the pending action changed.
	 *
	 * @return the pending action changed
	 */
	public Boolean getPendingActionChanged() {

		return pendingActionChanged;
	}

	/**
	 * Sets the pending action changed.
	 *
	 * @param pendingActionChanged the new pending action changed
	 */
	public void setPendingActionChanged(Boolean pendingActionChanged) {

		this.pendingActionChanged = pendingActionChanged;
	}

	/**
	 * Gets the available date.
	 *
	 * @return the available date
	 */
	public Date getAvailableDate() {

		return availableDate;
	}

	/**
	 * Sets the available date.
	 *
	 * @param availableDate the new available date
	 */
	public void setAvailableDate(Date availableDate) {

		this.availableDate = availableDate;
	}

	/**
	 * Gets the owner.
	 *
	 * @return the owner
	 */
	public String getOwner() {

		return owner;
	}

	/**
	 * Sets the owner.
	 *
	 * @param owner the new owner
	 */
	public void setOwner(String owner) {

		this.owner = owner;
	}

	/**
	 * Gets the owner name.
	 *
	 * @return the owner name
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the new owner name
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the group owner.
	 *
	 * @return the group owner
	 */
	public String getGroupOwner() {

		return groupOwner;
	}

	/**
	 * Sets the group owner.
	 *
	 * @param groupOwner the new group owner
	 */
	public void setGroupOwner(String groupOwner) {

		this.groupOwner = groupOwner;
	}

	/**
	 * Gets the group owner name.
	 *
	 * @return the group owner name
	 */
	public String getGroupOwnerName() {

		return groupOwnerName;
	}

	/**
	 * Sets the group owner name.
	 *
	 * @param groupOwnerName the new group owner name
	 */
	public void setGroupOwnerName(String groupOwnerName) {

		this.groupOwnerName = groupOwnerName;
	}

	/**
	 * Gets the collection type.
	 *
	 * @return the collection type
	 */
	public String getCollectionType() {

		return collectionType;
	}

	/**
	 * Sets the collection type.
	 *
	 * @param collectionType the new collection type
	 */
	public void setCollectionType(String collectionType) {

		this.collectionType = collectionType;
	}

	/**
	 * Gets the id parent collection.
	 *
	 * @return the idParentCollection
	 */
	public Long getIdParentCollection() {

		return idParentCollection;
	}

	/**
	 * Sets the id parent collection.
	 *
	 * @param idParentCollection the idParentCollection to set
	 */
	public void setIdParentCollection(Long idParentCollection) {

		this.idParentCollection = idParentCollection;
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
	 * Gets the statut libelle.
	 *
	 * @return the statut libelle
	 */
	public String getStatutLibelle() {

		return statutLibelle;
	}

	/**
	 * Sets the statut libelle.
	 *
	 * @param statutLibelle the new statut libelle
	 */
	public void setStatutLibelle(String statutLibelle) {

		this.statutLibelle = statutLibelle;
	}

	/**
	 * Gets the statut libelle done.
	 *
	 * @return the statut libelle done
	 */
	public String getStatutLibelleDone() {

		return statutLibelleDone;
	}

	/**
	 * Sets the statut libelle done.
	 *
	 * @param statutLibelleDone the new statut libelle done
	 */
	public void setStatutLibelleDone(String statutLibelleDone) {

		this.statutLibelleDone = statutLibelleDone;
	}

}
