/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.springframework.data.annotation.Transient;

import com.acm.utils.audit.AuditTrailListener;
import com.acm.utils.date.DateUtil;
import com.acm.utils.enums.CustomerMaritalStatus;
import com.acm.utils.enums.CustomerMezaCardStatus;

/**
 * The persistent class for the ACM_CUSTOMER table. {@link Customer} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.1
 */
@Entity
@Table(name = "ACM_CUSTOMER")
@EntityListeners(AuditTrailListener.class)
public class Customer extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8252918111601978289L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_CUSTOMER", unique = true, nullable = false)
	private Long id;

	/** The customer id extern. */
	@Column(name = "CUSTOMER_ID_EXTERN", nullable = false)
	private Long customerIdExtern;

	/** The person id extern. */
	@Column(name = "PERSON_ID_EXTERN")
	private Long personIdExtern;

	/** The customer number. */
	@Column(name = "CUSTOMER_NUMBER", length = 512)
	private String customerNumber;

	/** The open date. */
	@Column(name = "CUSTOMER_OPENDATE")
	private Date customerOpenDate;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME", length = 512)
	private String customerName;

	/** The correspondance name. */
	@Column(name = "CORRESPONDENCE_NAME", length = 512)
	private String correspondanceName;

	/** The alt name. */
	@Column(name = "ALT_NAME", length = 512)
	private String altName;

	/** The date of birth. */
	@Column(name = "DATE_OF_BIRTH", length = 512)
	private Date dateOfBirth;

	/** The age. */
	@Transient
	private Long age;

	/** The account portflio id. */
	@Column(name = "ACCOUNT_PORTFOLIO_ID", nullable = false)
	private Long accountPortfolioID;

	/** The portflio code. */
	@Column(name = "PORTFOLIO_CODE", length = 512)
	private String accountPortfolioCode;

	/** The portflio description. */
	@Column(name = "PORTFOLIO_DESCRIPTION", length = 512)
	private String accountPortfolioDescription;

	/** The adresse. */
	@Column(name = "CUSTOMER_ADDRESS", length = 512)
	private String customerAddress;

	/** The branchID. */
	@Column(name = "BRANCH_ID")
	private Integer branchId;

	/** The branch name. */
	@Column(name = "BRANCHE_NAME", length = 512)
	private String branchesName;

	/** The branch description. */
	@Column(name = "BRANCHE_DESCRIPTION", length = 512)
	private String branchesDescription;

	/** The telephone1. */
	@Column(name = "TELEPHONE_1", length = 256)
	private String telephone1;

	/** The telephone 2. */
	@Column(name = "TELEPHONE_2", length = 256)
	private String telephone2;

	/** The telephone 3. */
	@Column(name = "TELEPHONE_3", length = 256)
	private String telephone3;

	/** The customer type. */
	@Column(name = "CUSTOMER_TYPE", length = 215)
	private String customerType;

	/** The is customer. */
	@Column(name = "IS_CUSTOMER")
	private Boolean isCustomer;

	/** The organisation id extern. */
	@Column(name = "ORGANISATION_ID_EXTERN")
	private Long organisationIdExtern;

	/** The organisation id . */
	@Column(name = "ORGANISATION_ID")
	private Long organisationId;

	/** The register number. */
	@Column(name = "REGISTER_NUMBER")
	private String registerNumber;

	/** The fax. */
	@Column(name = "FAX")
	private String fax;

	/** The web site. */
	@Column(name = "WEB_SITE")
	private String webSite;

	/** The sector. */
	@Column(name = "SECTOR")
	private String sector;

	/** The email. */
	@Column(name = "EMAIL")
	private String email;

	/** The gender. */
	@Column(name = "GENDER")
	private String gender;

	/** The date of birth hijri. */
	@Column(name = "DATE_OF_BIRTH_HIJRI")
	private String dateOfBirthHijri;

	/** The organization name. */
	@Column(name = "ORGANIZATION_NAME")
	private String organizationName;

	/** The account year end. */
	@Column(name = "ACCOUNT_YEAR_END")
	private Date accountYearEnd;

	/** The solidarity name. */
	@Column(name = "SOLIDARITY_NAME")
	private String solidarityName;

	/** The update customer. */
	@Column(name = "UPDATE_CUSTOMER")
	private Boolean updateCustomer;

	/** The identity customer. */
	@Column(name = "IDENTITY_NUMBER")
	private String identity;

	/** The marital status : {@link CustomerMaritalStatus}. */
	@Column(name = "MARITAL_STATUS", length = 5)
	private String maritalStatus;

	/** The meza card status : {@link CustomerMezaCardStatus} . */
	@Column(name = "MEZA_CARD_STATUS", length = 50)
	private String mezaCardStatus;

	/** The photo. */
	@Column(name = "PHOTO")
	private byte[] photo;

	/** The loans. */
	@OneToMany(mappedBy = "customer")
	private Set<Loan> loans = new HashSet<>();

	/** The ib loans. */
	@OneToMany(mappedBy = "customer")
	private Set<IBLoan> ibLoans = new HashSet<>();

	/** The customer links relationships. */
	@OneToMany(mappedBy = "member")
	private Set<CustomerLinksRelationship> customerLinksRelationships = new HashSet<>();

	/** The acm meza cards. */
	@OneToMany(mappedBy = "customer")
	private Set<AcmMezaCard> acmMezaCards = new HashSet<>();

	/** The enable critical data. */
	@Column(name = "ENABLE_CRITICAL_DATA")
	private Boolean enableCriticalData;

	/** The ib customer id. */
	@Column(name = "IB_CUSTOMER_ID")
	private Long ibCustomerId;

	/** The is supplier. */
	@Column(name = "IS_SUPPLIER")
	private Boolean isSupplier;

	/** The beneficial effective. */
	@Column(name = "BENEFICIAL_EFFECTIVE")
	private String beneficialEffective;

	/** The prospection source. */
	@Column(name = "PROSPECTION_SOURCE")
	private String prospectionSource;

	/** The prospection comment. */
	@Column(name = "PROSPECTION_COMMENT")
	private String prospectionComment;

	/** The supplier recommandation. */
	@Column(name = "SUPPLIER_RECOMMANDATION")
	private Long supplierRecommandation;

	/**
	 * Instantiates a new customer.
	 */
	public Customer() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new customer.
	 *
	 * @param id the id
	 */
	public Customer(Long id) {

		this.id = id;
	}

	/**
	 * Instantiates a new customer.
	 *
	 * @param customerIdExtern the customer id extern
	 * @param customerNumber the customer number
	 * @param customerOpenDate the customer open date
	 * @param customerName the customer name
	 * @param correspondanceName the correspondance name
	 * @param altName the alt name
	 * @param dateOfBirth the date of birth
	 * @param accountPortfolioID the account portfolio ID
	 * @param accountPortfolioCode the account portfolio code
	 * @param accountPortfolioDescription the account portfolio description
	 * @param customerAddress the customer address
	 * @param branchId the branch id
	 * @param branchesName the branches name
	 * @param branchesDescription the branches description
	 * @param age the age
	 * @param telephone1 the telephone 1
	 * @param telephone2 the telephone 2
	 * @param telephone3 the telephone 3
	 * @param customerType the customer type
	 * @param isCustomer the is customer
	 * @param organisationId the organisation id
	 * @param organisationIdExtern the organisation id extern
	 * @param registerNumber the register number
	 * @param fax the fax
	 * @param webSite the web site
	 * @param sector the sector
	 * @param email the email
	 * @param isSupplier the is supplier
	 * @param beneficialEffective the beneficial effective
	 * @param prospectionSource the prospection source
	 * @param prospectionComment the prospection comment
	 * @param supplierRecommandation the supplier recommandation
	 */
	public Customer(Long customerIdExtern, String customerNumber, Date customerOpenDate,
			String customerName, String correspondanceName, String altName, Date dateOfBirth,
			Long accountPortfolioID, String accountPortfolioCode,
			String accountPortfolioDescription, String customerAddress, Integer branchId,
			String branchesName, String branchesDescription, Long age, String telephone1,
			String telephone2, String telephone3, String customerType, Boolean isCustomer,
			Long organisationId, Long organisationIdExtern, String registerNumber, String fax,
			String webSite, String sector, String email, Boolean isSupplier,
			String beneficialEffective, String prospectionSource, String prospectionComment,
			Long supplierRecommandation) {

		this.customerIdExtern = customerIdExtern;
		this.customerNumber = customerNumber;
		this.customerOpenDate = customerOpenDate;
		this.customerName = customerName;
		this.correspondanceName = correspondanceName;
		this.altName = altName;
		this.dateOfBirth = dateOfBirth;
		this.setAge(0L);
		this.accountPortfolioID = accountPortfolioID;
		this.accountPortfolioCode = accountPortfolioCode;
		this.accountPortfolioDescription = accountPortfolioDescription;
		this.customerAddress = customerAddress;
		this.branchId = branchId;
		this.branchesName = branchesName;
		this.branchesDescription = branchesDescription;
		this.telephone1 = telephone1;
		this.telephone2 = telephone2;
		this.telephone3 = telephone3;
		this.customerType = customerType;
		this.isCustomer = isCustomer;
		this.organisationId = organisationId;
		this.organisationIdExtern = organisationIdExtern;
		this.registerNumber = registerNumber;
		this.fax = fax;
		this.webSite = webSite;
		this.sector = sector;
		this.email = email;
		this.isSupplier = isSupplier;
		this.beneficialEffective = beneficialEffective;
		this.prospectionSource = prospectionSource;
		this.prospectionComment = prospectionComment;
		this.supplierRecommandation = supplierRecommandation;
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
	 * Gets the customer id extern.
	 *
	 * @return the customerIdExtern
	 */
	public Long getCustomerIdExtern() {

		return customerIdExtern;
	}

	/**
	 * Sets the customer id extern.
	 *
	 * @param customerIdExtern the customerIdExtern to set
	 */
	public void setCustomerIdExtern(Long customerIdExtern) {

		this.customerIdExtern = customerIdExtern;
	}

	/**
	 * Gets the correspondance name.
	 *
	 * @return the correspondanceName
	 */
	public String getCorrespondanceName() {

		return correspondanceName;
	}

	/**
	 * Sets the correspondance name.
	 *
	 * @param correspondanceName the correspondanceName to set
	 */
	public void setCorrespondanceName(String correspondanceName) {

		this.correspondanceName = correspondanceName;
	}

	/**
	 * Gets the alt name.
	 *
	 * @return the altName
	 */
	public String getAltName() {

		return altName;
	}

	/**
	 * Sets the alt name.
	 *
	 * @param altName the altName to set
	 */
	public void setAltName(String altName) {

		this.altName = altName;
	}

	/**
	 * Gets the date of birth.
	 *
	 * @return the dateOfBirth
	 */
	public Date getDateOfBirth() {

		return dateOfBirth;
	}

	/**
	 * Sets the date of birth.
	 *
	 * @param dateOfBirth the dateOfBirth to set
	 */
	public void setDateOfBirth(Date dateOfBirth) {

		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * Gets the age.
	 *
	 * @author nrmila
	 * @return the age
	 */
	public Long getAge() {

		return (long) DateUtil.calculateAge(this.dateOfBirth);
	}

	/**
	 * Sets the age.
	 *
	 * @author nrmila
	 * @param age the new age
	 */
	public void setAge(Long age) {

		this.age = (long) DateUtil.calculateAge(this.dateOfBirth);
	}

	/**
	 * Gets the branch id.
	 *
	 * @return the branchId
	 */
	public Integer getBranchId() {

		return branchId;
	}

	/**
	 * Sets the branch id.
	 *
	 * @param branchId the branchId to set
	 */
	public void setBranchId(Integer branchId) {

		this.branchId = branchId;
	}

	/**
	 * Gets the loans.
	 *
	 * @return the loans
	 */
	public Set<Loan> getLoans() {

		return loans;
	}

	/**
	 * Sets the loans.
	 *
	 * @param loans the loans to set
	 */
	public void setLoans(Set<Loan> loans) {

		this.loans = loans;
	}

	/**
	 * Gets the customer number.
	 *
	 * @return the customerNumber
	 */
	public String getCustomerNumber() {

		return customerNumber;
	}

	/**
	 * Sets the customer number.
	 *
	 * @param customerNumber the customerNumber to set
	 */
	public void setCustomerNumber(String customerNumber) {

		this.customerNumber = customerNumber;
	}

	/**
	 * Gets the customer open date.
	 *
	 * @return the customerOpenDate
	 */
	public Date getCustomerOpenDate() {

		return customerOpenDate;
	}

	/**
	 * Sets the customer open date.
	 *
	 * @param customerOpenDate the customerOpenDate to set
	 */
	public void setCustomerOpenDate(Date customerOpenDate) {

		this.customerOpenDate = customerOpenDate;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customerName
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the customerName to set
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the account portfolio ID.
	 *
	 * @return the accountPortfolioID
	 */
	public Long getAccountPortfolioID() {

		return accountPortfolioID;
	}

	/**
	 * Sets the account portfolio ID.
	 *
	 * @param accountPortfolioID the accountPortfolioID to set
	 */
	public void setAccountPortfolioID(Long accountPortfolioID) {

		this.accountPortfolioID = accountPortfolioID;
	}

	/**
	 * Gets the account portfolio code.
	 *
	 * @return the accountPortfolioCode
	 */
	public String getAccountPortfolioCode() {

		return accountPortfolioCode;
	}

	/**
	 * Sets the account portfolio code.
	 *
	 * @param accountPortfolioCode the accountPortfolioCode to set
	 */
	public void setAccountPortfolioCode(String accountPortfolioCode) {

		this.accountPortfolioCode = accountPortfolioCode;
	}

	/**
	 * Gets the account portfolio description.
	 *
	 * @return the accountPortfolioDescription
	 */
	public String getAccountPortfolioDescription() {

		return accountPortfolioDescription;
	}

	/**
	 * Sets the account portfolio description.
	 *
	 * @param accountPortfolioDescription the accountPortfolioDescription to set
	 */
	public void setAccountPortfolioDescription(String accountPortfolioDescription) {

		this.accountPortfolioDescription = accountPortfolioDescription;
	}

	/**
	 * Gets the customer address.
	 *
	 * @return the customerAddress
	 */
	public String getCustomerAddress() {

		return customerAddress;
	}

	/**
	 * Sets the customer address.
	 *
	 * @param customerAddress the customerAddress to set
	 */
	public void setCustomerAddress(String customerAddress) {

		this.customerAddress = customerAddress;
	}

	/**
	 * Gets the branches name.
	 *
	 * @return the branchesName
	 */
	public String getBranchesName() {

		return branchesName;
	}

	/**
	 * Sets the branches name.
	 *
	 * @param branchesName the branchesName to set
	 */
	public void setBranchesName(String branchesName) {

		this.branchesName = branchesName;
	}

	/**
	 * Gets the branches description.
	 *
	 * @return the branchesDescription
	 */
	public String getBranchesDescription() {

		return branchesDescription;
	}

	/**
	 * Sets the branches description.
	 *
	 * @param branchesDescription the branchesDescription to set
	 */
	public void setBranchesDescription(String branchesDescription) {

		this.branchesDescription = branchesDescription;
	}

	/**
	 * Gets the telephone 1.
	 *
	 * @return the telephone1
	 */
	public String getTelephone1() {

		return telephone1;
	}

	/**
	 * Sets the telephone 1.
	 *
	 * @param telephone1 the telephone1 to set
	 */
	public void setTelephone1(String telephone1) {

		this.telephone1 = telephone1;
	}

	/**
	 * Gets the telephone 2.
	 *
	 * @return the telephone2
	 */
	public String getTelephone2() {

		return telephone2;
	}

	/**
	 * Sets the telephone 2.
	 *
	 * @param telephone2 the telephone2 to set
	 */
	public void setTelephone2(String telephone2) {

		this.telephone2 = telephone2;
	}

	/**
	 * Gets the telephone 3.
	 *
	 * @return the telephone3
	 */
	public String getTelephone3() {

		return telephone3;
	}

	/**
	 * Sets the telephone 3.
	 *
	 * @param telephone3 the telephone3 to set
	 */
	public void setTelephone3(String telephone3) {

		this.telephone3 = telephone3;
	}

	/**
	 * Gets the customer type.
	 * 
	 * @return the customerType
	 */
	public String getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 * 
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(String customerType) {

		this.customerType = customerType;
	}

	/**
	 * Gets the organisation id extern.
	 *
	 * @return the organisationIdExtern
	 */
	public Long getOrganisationIdExtern() {

		return organisationIdExtern;
	}

	/**
	 * Sets the organisation id extern.
	 *
	 * @param organisationIdExtern the organisationIdExtern to set
	 */
	public void setOrganisationIdExtern(Long organisationIdExtern) {

		this.organisationIdExtern = organisationIdExtern;
	}

	/**
	 * Gets the organisation id.
	 *
	 * @return the organisationId
	 */
	public Long getOrganisationId() {

		return organisationId;
	}

	/**
	 * Sets the organisation id.
	 *
	 * @param organisationId the organisationId to set
	 */
	public void setOrganisationId(Long organisationId) {

		this.organisationId = organisationId;
	}

	/**
	 * Gets the register number.
	 *
	 * @return the registerNumber
	 */
	public String getRegisterNumber() {

		return registerNumber;
	}

	/**
	 * Sets the register number.
	 *
	 * @param registerNumber the registerNumber to set
	 */
	public void setRegisterNumber(String registerNumber) {

		this.registerNumber = registerNumber;
	}

	/**
	 * Gets the fax.
	 *
	 * @return the fax
	 */
	public String getFax() {

		return fax;
	}

	/**
	 * Sets the fax.
	 *
	 * @param fax the fax to set
	 */
	public void setFax(String fax) {

		this.fax = fax;
	}

	/**
	 * Gets the web site.
	 *
	 * @return the webSite
	 */
	public String getWebSite() {

		return webSite;
	}

	/**
	 * Sets the web site.
	 *
	 * @param webSite the webSite to set
	 */
	public void setWebSite(String webSite) {

		this.webSite = webSite;
	}

	/**
	 * Gets the sector.
	 *
	 * @return the sector
	 */
	public String getSector() {

		return sector;
	}

	/**
	 * Sets the sector.
	 *
	 * @param sector the sector to set
	 */
	public void setSector(String sector) {

		this.sector = sector;
	}

	/**
	 * Gets the email.
	 * 
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Sets the email.
	 * 
	 * @param email the email to set
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the ib loans.
	 *
	 * @return the ibLoans
	 */
	public Set<IBLoan> getIbLoans() {

		return ibLoans;
	}

	/**
	 * Sets the ib loans.
	 *
	 * @param ibLoans the ibLoans to set
	 */
	public void setIbLoans(Set<IBLoan> ibLoans) {

		this.ibLoans = ibLoans;
	}

	/**
	 * Gets the gender.
	 *
	 * @return the gender
	 */
	public String getGender() {

		return gender;
	}

	/**
	 * Sets the gender.
	 *
	 * @param gender the gender to set
	 */
	public void setGender(String gender) {

		this.gender = gender;
	}

	/**
	 * Gets the date of birth hijri.
	 *
	 * @return the dateOfBirthHijri
	 */
	public String getDateOfBirthHijri() {

		return dateOfBirthHijri;
	}

	/**
	 * Sets the date of birth hijri.
	 *
	 * @param dateOfBirthHijri the dateOfBirthHijri to set
	 */
	public void setDateOfBirthHijri(String dateOfBirthHijri) {

		this.dateOfBirthHijri = dateOfBirthHijri;
	}

	/**
	 * Gets the organization name.
	 *
	 * @return the organizationName
	 */
	public String getOrganizationName() {

		return organizationName;
	}

	/**
	 * Sets the organization name.
	 *
	 * @param organizationName the organizationName to set
	 */
	public void setOrganizationName(String organizationName) {

		this.organizationName = organizationName;
	}

	/**
	 * Gets the account year end.
	 *
	 * @return the accountYearEnd
	 */
	public Date getAccountYearEnd() {

		return accountYearEnd;
	}

	/**
	 * Sets the account year end.
	 *
	 * @param accountYearEnd the accountYearEnd to set
	 */
	public void setAccountYearEnd(Date accountYearEnd) {

		this.accountYearEnd = accountYearEnd;
	}

	/**
	 * Gets the solidarity name.
	 *
	 * @return the solidarityName
	 */
	public String getSolidarityName() {

		return solidarityName;
	}

	/**
	 * Sets the solidarity name.
	 *
	 * @param solidarityName the solidarityName to set
	 */
	public void setSolidarityName(String solidarityName) {

		this.solidarityName = solidarityName;
	}

	/**
	 * Gets the customer links relationships.
	 *
	 * @return the customerLinksRelationships
	 */
	public Set<CustomerLinksRelationship> getCustomerLinksRelationships() {

		return customerLinksRelationships;
	}

	/**
	 * Sets the customer links relationships.
	 *
	 * @param customerLinksRelationships the customerLinksRelationships to set
	 */
	public void setCustomerLinksRelationships(
			Set<CustomerLinksRelationship> customerLinksRelationships) {

		this.customerLinksRelationships = customerLinksRelationships;
	}

	/**
	 * Gets the update customer.
	 *
	 * @return the updateCustomer
	 */
	public Boolean getUpdateCustomer() {

		return updateCustomer;
	}

	/**
	 * Sets the update customer.
	 *
	 * @param updateCustomer the updateCustomer to set
	 */
	public void setUpdateCustomer(Boolean updateCustomer) {

		this.updateCustomer = updateCustomer;
	}

	/**
	 * Gets the checks if is customer.
	 *
	 * @return the isCustomer
	 */
	public Boolean getIsCustomer() {

		return isCustomer;
	}

	/**
	 * Sets the checks if is customer.
	 *
	 * @param isCustomer the isCustomer to set
	 */
	public void setIsCustomer(Boolean isCustomer) {

		this.isCustomer = isCustomer;
	}

	/**
	 * Gets the person id extern.
	 *
	 * @return the personIdExtern
	 */
	public Long getPersonIdExtern() {

		return personIdExtern;
	}

	/**
	 * Sets the person id extern.
	 *
	 * @param personIdExtern the personIdExtern to set
	 */
	public void setPersonIdExtern(Long personIdExtern) {

		this.personIdExtern = personIdExtern;
	}

	/**
	 * Gets the identity.
	 *
	 * @return the identity
	 */
	public String getIdentity() {

		return identity;
	}

	/**
	 * Sets the identity.
	 *
	 * @param identity the new identity
	 */
	public void setIdentity(String identity) {

		this.identity = identity;
	}

	/**
	 * Gets the marital status.
	 *
	 * @return the maritalStatus
	 */
	public String getMaritalStatus() {

		return maritalStatus;
	}

	/**
	 * Sets the marital status.
	 *
	 * @param maritalStatus the maritalStatus to set
	 */
	public void setMaritalStatus(String maritalStatus) {

		this.maritalStatus = maritalStatus;
	}

	/**
	 * Gets the photo.
	 *
	 * @return the photo
	 */
	public byte[] getPhoto() {

		return photo;
	}

	/**
	 * Sets the photo.
	 *
	 * @param photo the new photo
	 */
	public void setPhoto(byte[] photo) {

		this.photo = photo;
	}

	/**
	 * Gets the meza card status.
	 *
	 * @return the mezaCardStatus
	 */
	public String getMezaCardStatus() {

		return mezaCardStatus;
	}

	/**
	 * Sets the meza card status.
	 *
	 * @param mezaCardStatus the mezaCardStatus to set
	 */
	public void setMezaCardStatus(String mezaCardStatus) {

		this.mezaCardStatus = mezaCardStatus;
	}

	/**
	 * Gets the acm meza cards.
	 *
	 * @return the acm meza cards
	 */
	public Set<AcmMezaCard> getAcmMezaCards() {

		return acmMezaCards;
	}

	/**
	 * Sets the acm meza cards.
	 *
	 * @param acmMezaCards the new acm meza cards
	 */
	public void setAcmMezaCards(Set<AcmMezaCard> acmMezaCards) {

		this.acmMezaCards = acmMezaCards;
	}

	/**
	 * Gets the enable critical data.
	 *
	 * @return the enable critical data
	 */
	public Boolean getEnableCriticalData() {

		return enableCriticalData;
	}

	/**
	 * Sets the enable critical data.
	 *
	 * @param enableCriticalData the new enable critical data
	 */
	public void setEnableCriticalData(Boolean enableCriticalData) {

		this.enableCriticalData = enableCriticalData;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.utils.models.GenericModel#toString()
	 */
	@Override
	public String toString() {

		return "Customer [id=" + id + ", customerIdExtern=" + customerIdExtern + ", personIdExtern="
				+ personIdExtern + ", customerNumber=" + customerNumber + ", customerOpenDate="
				+ customerOpenDate + ", customerName=" + customerName + ", correspondanceName="
				+ correspondanceName + ", altName=" + altName + ", dateOfBirth=" + dateOfBirth
				+ ", age=" + age + ", accountPortfolioID=" + accountPortfolioID
				+ ", accountPortfolioCode=" + accountPortfolioCode
				+ ", accountPortfolioDescription=" + accountPortfolioDescription
				+ ", customerAddress=" + customerAddress + ", branchId=" + branchId
				+ ", branchesName=" + branchesName + ", branchesDescription=" + branchesDescription
				+ ", telephone1=" + telephone1 + ", telephone2=" + telephone2 + ", telephone3="
				+ telephone3 + ", customerType=" + customerType + ", isCustomer=" + isCustomer
				+ ", organisationIdExtern=" + organisationIdExtern + ", organisationId="
				+ organisationId + ", registerNumber=" + registerNumber + ", fax=" + fax
				+ ", webSite=" + webSite + ", sector=" + sector + ", email=" + email + ", gender="
				+ gender + ", dateOfBirthHijri=" + dateOfBirthHijri + ", organizationName="
				+ organizationName + ", accountYearEnd=" + accountYearEnd + ", solidarityName="
				+ solidarityName + ", updateCustomer=" + updateCustomer + ", identity=" + identity
				+ ", maritalStatus=" + maritalStatus + ", mezaCardStatus=" + mezaCardStatus
				+ ", photo=" + Arrays.toString(photo) + ", loans=" + loans + ", ibLoans=" + ibLoans
				+ ", customerLinksRelationships=" + customerLinksRelationships + ", acmMezaCards="
				+ acmMezaCards + ", enableCriticalData=" + enableCriticalData + ", ibCustomerId="
				+ ibCustomerId + "]";
	}

	/**
	 * Gets the ib customer id.
	 *
	 * @return the ib customer id
	 */
	public Long getIbCustomerId() {

		return ibCustomerId;
	}

	/**
	 * Sets the ib customer id.
	 *
	 * @param ibCustomerId the new ib customer id
	 */
	public void setIbCustomerId(Long ibCustomerId) {

		this.ibCustomerId = ibCustomerId;
	}

	/**
	 * Gets the checks if is supplier.
	 *
	 * @return the checks if is supplier
	 */
	public Boolean getIsSupplier() {

		return isSupplier;
	}

	/**
	 * Sets the checks if is supplier.
	 *
	 * @param isSupplier the new checks if is supplier
	 */
	public void setIsSupplier(Boolean isSupplier) {

		this.isSupplier = isSupplier;
	}

	/**
	 * Gets the beneficial effective.
	 *
	 * @return the beneficial effective
	 */
	public String getBeneficialEffective() {

		return beneficialEffective;
	}

	/**
	 * Sets the beneficial effective.
	 *
	 * @param beneficialEffective the new beneficial effective
	 */
	public void setBeneficialEffective(String beneficialEffective) {

		this.beneficialEffective = beneficialEffective;
	}

	/**
	 * Gets the prospection source.
	 *
	 * @return the prospection source
	 */
	public String getProspectionSource() {

		return prospectionSource;
	}

	/**
	 * Sets the prospection source.
	 *
	 * @param prospectionSource the new prospection source
	 */
	public void setProspectionSource(String prospectionSource) {

		this.prospectionSource = prospectionSource;
	}

	/**
	 * Gets the prospection comment.
	 *
	 * @return the prospection comment
	 */
	public String getProspectionComment() {

		return prospectionComment;
	}

	/**
	 * Sets the prospection comment.
	 *
	 * @param prospectionComment the new prospection comment
	 */
	public void setProspectionComment(String prospectionComment) {

		this.prospectionComment = prospectionComment;
	}

	/**
	 * Gets the supplier recommandation.
	 *
	 * @return the supplier recommandation
	 */
	public Long getSupplierRecommandation() {

		return supplierRecommandation;
	}

	/**
	 * Sets the supplier recommandation.
	 *
	 * @param supplierRecommandation the new supplier recommandation
	 */
	public void setSupplierRecommandation(Long supplierRecommandation) {

		this.supplierRecommandation = supplierRecommandation;
	}

}
