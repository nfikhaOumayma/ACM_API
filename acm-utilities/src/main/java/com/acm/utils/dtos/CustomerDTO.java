/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import org.springframework.data.annotation.Transient;

import com.acm.utils.date.DateUtil;
import com.acm.utils.enums.CustomerMaritalStatus;
import com.acm.utils.enums.CustomerMezaCardStatus;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link CustomerDTO} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class CustomerDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8316364978249763683L;

	/** The id. */
	private Long id;

	/** The customer id. */
	private Long customerIdExtern;

	/** The person id extern. */
	private Long personIdExtern;

	/** The customer number. */
	private String customerNumber;

	/** The customer name. */
	private String customerName;

	/** The customer name no pipe. */
	private String customerNameNoPipe;

	/** The correspondance name. */
	private String correspondanceName;

	/** The alt name. */
	private String altName;

	/** The customer open date. */
	private Date customerOpenDate;

	/** The date of birth. */
	private Date dateOfBirth;

	/** The account portfolio ID. */
	private Long accountPortfolioID;

	/** The account portfolio code. */
	private String accountPortfolioCode;

	/** The account portfolio description. */
	private String accountPortfolioDescription;

	/** The customer address. */
	private String customerAddress;

	/** The branch id. */
	private Integer branchId;

	/** The branches name. */
	private String branchesName;

	/** The branches description. */
	private String branchesDescription;

	/** The age. */
	@Transient
	private Long age;

	/** The arrear day. */
	private Long arrearDay;

	/** The arrear schedule. */
	private Long arrearSchedule;

	/** the telephone. */
	private String telephone;

	/** The telephone. */
	private String telephone1;

	/** The telephone 2. */
	private String telephone2;

	/** The telephone 3. */
	private String telephone3;

	/** The customer type. */
	private String customerType;

	/** The is customer. */
	private Boolean isCustomer;

	/** The organisation id. */
	private Long organisationId;

	/** The organisation id extern. */
	private Long organisationIdExtern;

	/** The register number. */
	private String registerNumber;

	/** The fax. */
	private String fax;

	/** The web site. */
	private String webSite;

	/** The sector. */
	private String sector;

	/** The email. */
	private String email;

	/** The first name. */
	private String firstName;

	/** The second name. */
	private String secondName;

	/** The middle name. */
	private String middleName;

	/** The last name. */
	private String lastName;

	/** The gender. */
	private String gender;

	/** The date of birth hijri. */
	private String dateOfBirthHijri;

	/** The organization name. */
	private String organizationName;

	/** The account year end. */
	private Date accountYearEnd;

	/** The solidarity name. */
	private String solidarityName;

	/** The customer update. */
	private Boolean updateCustomer;

	/** The amount guarantor. */
	private BigDecimal amountGuarantor;

	/** The identity. */
	private String identity;

	/** The search form document. */
	private Boolean searchFormDocument;

	/** The industry code. */
	private IndustryDTO industryCode;

	/** The customer link category : GUARANTOR / GRP / ORG / RELATIONSHIP. */
	private String customerLinkCategory;

	/** The marital status : {@link CustomerMaritalStatus}. */
	private String maritalStatus;

	/** The meza card status : {@link CustomerMezaCardStatus} . */
	private String mezaCardStatus;

	/** The customer links relationship DTOs. */
	private List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs;

	/** The customer links DTOs. */
	private List<CustomerLinksRelationshipDTO> customerLinksDTOs;

	/** The address DT os. */
	private List<AddressDTO> listAddress;

	/** The user defined fields links DT os. */
	private List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs;

	/** The user defined fields links DT os. */
	private List<CustomerDTO> guarantors;

	/** The meza card account number. */
	private Long mezaCardId;

	/** The acm meza card DTO. */
	private AcmMezaCardDTO acmMezaCardDTO;

	/** The disbursement method updated to other than meza card. */
	private Boolean disbursementMethodUpdatedToOtherThanMezaCard;

	/** The disbursement method selected. */
	private String disbursementMethodSelected;

	/** The date insertion. */
	private Date dateInsertion;

	/** The enabled. */
	private Boolean enabled;

	/** The action. */
	private String action;

	/** The enable critical data. */
	private Boolean enableCriticalData;

	/** The role. */
	private String role;

	/** The updated by. */
	private String updatedBy;

	/** The ib customer id. */
	private Long ibCustomerId;

	/** The udf links groupe fields DT os. */
	private List<UDFLinksGroupeFieldsDTO> udfLinksGroupeFieldsDTOs;

	/** The check customer. */
	private Boolean checkCustomer; // if false : do not check required fields and nationalId
									// duplication in save/update customer (this is only in case of
									// create customer from abacus via rabbitMq)

	/** The is supplier. */
	private Boolean isSupplier;

	/** The beneficial effective. */
	private String beneficialEffective;

	/** The prospection source. */
	private String prospectionSource;

	/** The prospection comment. */
	private String prospectionComment;

	/** The supplier recommandation. */
	private Long supplierRecommandation;

	/**
	 * Instantiates a new customer DTO.
	 */
	public CustomerDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer DTO.
	 * 
	 * @param customerName the customer name
	 */
	public CustomerDTO(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Instantiates a new customer DTO.
	 *
	 * @param customerIdExtern the customer id extern
	 */
	public CustomerDTO(Long customerIdExtern) {

		this.customerIdExtern = customerIdExtern;
	}

	/**
	 * Instantiates a new CustomerDTO.
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
	 * @param age the customer age
	 * @param telephone1 the telephone 1
	 * @param telephone2 the telephone 2
	 * @param telephone3 the telephone 3
	 * @param customerType the customer type
	 * @param isCustomer the customer category
	 * @param organisationId the organisation id
	 * @param beneficialEffective the beneficial effective
	 * @param prospectionSource the prospection source
	 * @param prospectionComment the prospection comment
	 * @param supplierRecommandation the supplier recommandation
	 */
	public CustomerDTO(Long customerIdExtern, String customerNumber, Date customerOpenDate,
			String customerName, String correspondanceName, String altName, Date dateOfBirth,
			Long accountPortfolioID, String accountPortfolioCode,
			String accountPortfolioDescription, String customerAddress, Integer branchId,
			String branchesName, String branchesDescription, Long age, String telephone1,
			String telephone2, String telephone3, String customerType, Boolean isCustomer,
			Long organisationId, String beneficialEffective, String prospectionSource,
			String prospectionComment, Long supplierRecommandation) {

		this.customerIdExtern = customerIdExtern;
		this.customerNumber = customerNumber;
		this.customerOpenDate = customerOpenDate;
		this.customerName = customerName;
		this.correspondanceName = correspondanceName;
		this.altName = altName;
		this.dateOfBirth = dateOfBirth;
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
		this.beneficialEffective = beneficialEffective;
		this.setAge(0L);
		this.prospectionSource = prospectionSource;
		this.prospectionComment = prospectionComment;
		this.supplierRecommandation = supplierRecommandation;
	}

	/**
	 * Gets the age.
	 *
	 * @return the age
	 */
	public Long getAge() {

		return (long) DateUtil.calculateAge(this.dateOfBirth);
	}

	/**
	 * Sets the age.
	 *
	 * @param age the new age
	 */
	public void setAge(Long age) {

		this.age = (long) DateUtil.calculateAge(this.dateOfBirth);
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
	 * Gets the arrear day.
	 *
	 * @return the arrearDay
	 */
	public Long getArrearDay() {

		return arrearDay;
	}

	/**
	 * Sets the arrear day.
	 *
	 * @param arrearDay the arrearDay to set
	 */
	public void setArrearDay(Long arrearDay) {

		this.arrearDay = arrearDay;
	}

	/**
	 * Gets the arrear schedule.
	 *
	 * @return the arrearSchedule
	 */
	public Long getArrearSchedule() {

		return arrearSchedule;
	}

	/**
	 * Sets the arrear schedule.
	 *
	 * @param arrearSchedule the arrearSchedule to set
	 */
	public void setArrearSchedule(Long arrearSchedule) {

		this.arrearSchedule = arrearSchedule;
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
	 * Sets the email.
	 * 
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Gets the email.
	 * 
	 * @param email the email to set
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the first name.
	 *
	 * @return the firstName
	 */
	public String getFirstName() {

		if (!ACMValidationUtils.isNullOrEmpty(this.customerName)) {
			String[] nameSurName = StringUtils.convertingString(this.customerName, "\\|");
			if (nameSurName.length >= 3) {
				firstName = nameSurName[0];
			}
		}
		return ACMValidationUtils.isNullOrEmpty(this.firstName) ? "" : firstName;
	}

	/**
	 * Sets the first name.
	 *
	 * @param firstName the firstName to set
	 */
	public void setFirstName(String firstName) {

		this.firstName = firstName;
	}

	/**
	 * Gets the second name.
	 *
	 * @return the secondName
	 */
	public String getSecondName() {

		if (!ACMValidationUtils.isNullOrEmpty(this.customerName)) {
			String[] nameSurName = StringUtils.convertingString(this.customerName, "\\|");
			if (nameSurName.length >= 3) {
				secondName = nameSurName[1];
			}
		}
		return ACMValidationUtils.isNullOrEmpty(this.secondName) ? "" : secondName;
	}

	/**
	 * Sets the second name.
	 *
	 * @param secondName the secondName to set
	 */
	public void setSecondName(String secondName) {

		this.secondName = secondName;
	}

	/**
	 * Gets the middle name.
	 *
	 * @return the middleName
	 */
	public String getMiddleName() {

		if (!ACMValidationUtils.isNullOrEmpty(this.customerName)) {
			String[] nameSurName = StringUtils.convertingString(this.customerName, "\\|");
			if (nameSurName.length >= 3) {
				middleName = nameSurName[2];
			}
		}
		return ACMValidationUtils.isNullOrEmpty(this.middleName) ? "" : middleName;
	}

	/**
	 * Sets the middle name.
	 *
	 * @param middleName the middleName to set
	 */
	public void setMiddleName(String middleName) {

		this.middleName = middleName;
	}

	/**
	 * Gets the last name.
	 *
	 * @return the lastName
	 */
	public String getLastName() {

		if (!ACMValidationUtils.isNullOrEmpty(this.customerName)) {
			String[] nameSurName = StringUtils.convertingString(this.customerName, "\\|");
			if (nameSurName.length > 3) {
				lastName = nameSurName[3];
			}
		}
		return ACMValidationUtils.isNullOrEmpty(this.lastName) ? "" : lastName;
	}

	/**
	 * Sets the last name.
	 *
	 * @param lastName the lastName to set
	 */
	public void setLastName(String lastName) {

		this.lastName = lastName;
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
	 * Gets the customer links relationship DT os.
	 *
	 * @return the customerLinksRelationshipDTOs
	 */
	public List<CustomerLinksRelationshipDTO> getCustomerLinksRelationshipDTOs() {

		return customerLinksRelationshipDTOs;
	}

	/**
	 * Sets the customer links relationship DT os.
	 *
	 * @param customerLinksRelationshipDTOs the customerLinksRelationshipDTOs to set
	 */
	public void setCustomerLinksRelationshipDTOs(
			List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs) {

		this.customerLinksRelationshipDTOs = customerLinksRelationshipDTOs;
	}

	/**
	 * Gets the list address.
	 *
	 * @return the listAddress
	 */
	public List<AddressDTO> getListAddress() {

		return listAddress;
	}

	/**
	 * Sets the list address.
	 *
	 * @param listAddress the listAddress to set
	 */
	public void setListAddress(List<AddressDTO> listAddress) {

		this.listAddress = listAddress;
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
	 * Gets the user defined fields links DT os.
	 *
	 * @return the userDefinedFieldsLinksDTOs
	 */
	public List<UserDefinedFieldsLinksDTO> getUserDefinedFieldsLinksDTOs() {

		return userDefinedFieldsLinksDTOs;
	}

	/**
	 * Sets the user defined fields links DT os.
	 *
	 * @param userDefinedFieldsLinksDTOs the userDefinedFieldsLinksDTOs to set
	 */
	public void setUserDefinedFieldsLinksDTOs(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		this.userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs;
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
	 * Gets the amount guarantor.
	 *
	 * @return the amountGuarantor
	 */
	public BigDecimal getAmountGuarantor() {

		return amountGuarantor;
	}

	/**
	 * Sets the amount guarantor.
	 *
	 * @param amountGuarantor the amountGuarantor to set
	 */
	public void setAmountGuarantor(BigDecimal amountGuarantor) {

		this.amountGuarantor = amountGuarantor;
	}

	/**
	 * Gets the telephone.
	 *
	 * @return the telephone
	 */
	public String getTelephone() {

		if (!ACMValidationUtils.isNullOrEmpty(this.telephone1)) {
			this.telephone = this.telephone1;
		}
		if (!ACMValidationUtils.isNullOrEmpty(this.telephone2)) {
			if (!ACMValidationUtils.isNullOrEmpty(this.telephone1)) {
				this.telephone += ", " + this.telephone2;
			}
			else {
				this.telephone += this.telephone2;
			}
		}
		if (!ACMValidationUtils.isNullOrEmpty(this.telephone3)) {
			if (!ACMValidationUtils.isNullOrEmpty(this.telephone1)
					|| !ACMValidationUtils.isNullOrEmpty(this.telephone2)) {
				this.telephone += ", " + this.telephone3;
			}
			else {
				this.telephone += this.telephone3;
			}
		}
		return telephone;
	}

	/**
	 * Sets the telephone.
	 *
	 * @param telephone the telephone to set
	 */
	public void setTelephone(String telephone) {

		this.telephone = telephone;
	}

	/**
	 * Gets the customer links DT os.
	 *
	 * @return the customerLinksDTOs
	 */
	public List<CustomerLinksRelationshipDTO> getCustomerLinksDTOs() {

		return customerLinksDTOs;
	}

	/**
	 * Sets the customer links DT os.
	 *
	 * @param customerLinksDTOs the customerLinksDTOs to set
	 */
	public void setCustomerLinksDTOs(List<CustomerLinksRelationshipDTO> customerLinksDTOs) {

		this.customerLinksDTOs = customerLinksDTOs;
	}

	/**
	 * Gets the guarantors.
	 *
	 * @return the guarantors
	 */
	public List<CustomerDTO> getGuarantors() {

		return guarantors;
	}

	/**
	 * Sets the guarantors.
	 *
	 * @param guarantors the guarantors to set
	 */
	public void setGuarantors(List<CustomerDTO> guarantors) {

		this.guarantors = guarantors;
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
	 * Gets the industry code.
	 *
	 * @return the industryCode
	 */
	public IndustryDTO getIndustryCode() {

		return industryCode;
	}

	/**
	 * Sets the industry code.
	 *
	 * @param industryCode the industryCode to set
	 */
	public void setIndustryCode(IndustryDTO industryCode) {

		this.industryCode = industryCode;
	}

	/**
	 * Gets the customer link category.
	 *
	 * @return the customerLinkCategory
	 */
	public String getCustomerLinkCategory() {

		return customerLinkCategory;
	}

	/**
	 * Sets the customer link category.
	 *
	 * @param customerLinkCategory the customerLinkCategory to set
	 */
	public void setCustomerLinkCategory(String customerLinkCategory) {

		this.customerLinkCategory = customerLinkCategory;
	}

	/**
	 * Gets the customer name no pipe.
	 *
	 * @return the customerNameNoPipe
	 */
	public String getCustomerNameNoPipe() {

		return customerNameNoPipe;
	}

	/**
	 * Sets the customer name no pipe.
	 *
	 * @param customerNameNoPipe the customerNameNoPipe to set
	 */
	public void setCustomerNameNoPipe(String customerNameNoPipe) {

		this.customerNameNoPipe = customerNameNoPipe;
	}

	/**
	 * Gets the search form document.
	 *
	 * @return the searchFormDocument
	 */
	public Boolean getSearchFormDocument() {

		return searchFormDocument;
	}

	/**
	 * Sets the search form document.
	 *
	 * @param searchFormDocument the searchFormDocument to set
	 */
	public void setSearchFormDocument(Boolean searchFormDocument) {

		this.searchFormDocument = searchFormDocument;
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
	 * Gets the meza card id.
	 *
	 * @return the meza card id
	 */
	public Long getMezaCardId() {

		return mezaCardId;
	}

	/**
	 * Sets the meza card id.
	 *
	 * @param mezaCardId the new meza card id
	 */
	public void setMezaCardId(Long mezaCardId) {

		this.mezaCardId = mezaCardId;
	}

	/**
	 * Gets the disbursement method updated to other than meza card.
	 *
	 * @return the disbursement method updated to other than meza card
	 */
	public Boolean getDisbursementMethodUpdatedToOtherThanMezaCard() {

		return disbursementMethodUpdatedToOtherThanMezaCard;
	}

	/**
	 * Sets the disbursement method updated to other than meza card.
	 *
	 * @param disbursementMethodUpdatedToOtherThanMezaCard the new disbursement method updated to
	 *        other than meza card
	 */
	public void setDisbursementMethodUpdatedToOtherThanMezaCard(
			Boolean disbursementMethodUpdatedToOtherThanMezaCard) {

		this.disbursementMethodUpdatedToOtherThanMezaCard =
				disbursementMethodUpdatedToOtherThanMezaCard;
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
	 * Gets the acm meza card DTO.
	 *
	 * @return the acmMezaCardDTO
	 */
	public AcmMezaCardDTO getAcmMezaCardDTO() {

		return acmMezaCardDTO;
	}

	/**
	 * Sets the acm meza card DTO.
	 *
	 * @param acmMezaCardDTO the acmMezaCardDTO to set
	 */
	public void setAcmMezaCardDTO(AcmMezaCardDTO acmMezaCardDTO) {

		this.acmMezaCardDTO = acmMezaCardDTO;
	}

	/**
	 * Gets the disbursement method selected.
	 *
	 * @return the disbursement method selected
	 */
	public String getDisbursementMethodSelected() {

		return disbursementMethodSelected;
	}

	/**
	 * Sets the disbursement method selected.
	 *
	 * @param disbursementMethodSelected the new disbursement method selected
	 */
	public void setDisbursementMethodSelected(String disbursementMethodSelected) {

		this.disbursementMethodSelected = disbursementMethodSelected;
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
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the role.
	 *
	 * @return the role
	 */
	public String getRole() {

		return role;
	}

	/**
	 * Sets the role.
	 *
	 * @param role the new role
	 */
	public void setRole(String role) {

		this.role = role;
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
	 * Gets the udf links groupe fields DT os.
	 *
	 * @return the udf links groupe fields DT os
	 */
	public List<UDFLinksGroupeFieldsDTO> getUdfLinksGroupeFieldsDTOs() {

		return udfLinksGroupeFieldsDTOs;
	}

	/**
	 * Sets the udf links groupe fields DT os.
	 *
	 * @param udfLinksGroupeFieldsDTOs the new udf links groupe fields DT os
	 */
	public void setUdfLinksGroupeFieldsDTOs(
			List<UDFLinksGroupeFieldsDTO> udfLinksGroupeFieldsDTOs) {

		this.udfLinksGroupeFieldsDTOs = udfLinksGroupeFieldsDTOs;
	}

	/**
	 * Gets the check customer.
	 *
	 * @return the check customer
	 */
	public Boolean getCheckCustomer() {

		return checkCustomer;
	}

	/**
	 * Sets the check customer.
	 *
	 * @param checkCustomer the new check customer
	 */
	public void setCheckCustomer(Boolean checkCustomer) {

		this.checkCustomer = checkCustomer;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerDTO [id=" + id + ", customerIdExtern=" + customerIdExtern
				+ ", personIdExtern=" + personIdExtern + ", customerNumber=" + customerNumber
				+ ", customerName=" + customerName + ", customerNameNoPipe=" + customerNameNoPipe
				+ ", correspondanceName=" + correspondanceName + ", altName=" + altName
				+ ", customerOpenDate=" + customerOpenDate + ", dateOfBirth=" + dateOfBirth
				+ ", accountPortfolioID=" + accountPortfolioID + ", accountPortfolioCode="
				+ accountPortfolioCode + ", accountPortfolioDescription="
				+ accountPortfolioDescription + ", customerAddress=" + customerAddress
				+ ", branchId=" + branchId + ", branchesName=" + branchesName
				+ ", branchesDescription=" + branchesDescription + ", age=" + age + ", arrearDay="
				+ arrearDay + ", arrearSchedule=" + arrearSchedule + ", telephone=" + telephone
				+ ", telephone1=" + telephone1 + ", telephone2=" + telephone2 + ", telephone3="
				+ telephone3 + ", customerType=" + customerType + ", isCustomer=" + isCustomer
				+ ", organisationId=" + organisationId + ", organisationIdExtern="
				+ organisationIdExtern + ", registerNumber=" + registerNumber + ", fax=" + fax
				+ ", webSite=" + webSite + ", sector=" + sector + ", email=" + email
				+ ", firstName=" + firstName + ", secondName=" + secondName + ", middleName="
				+ middleName + ", lastName=" + lastName + ", gender=" + gender
				+ ", dateOfBirthHijri=" + dateOfBirthHijri + ", organizationName="
				+ organizationName + ", accountYearEnd=" + accountYearEnd + ", solidarityName="
				+ solidarityName + ", updateCustomer=" + updateCustomer + ", amountGuarantor="
				+ amountGuarantor + ", identity=" + identity + ", searchFormDocument="
				+ searchFormDocument + ", industryCode=" + industryCode + ", customerLinkCategory="
				+ customerLinkCategory + ", maritalStatus=" + maritalStatus + ", mezaCardStatus="
				+ mezaCardStatus + ", customerLinksRelationshipDTOs="
				+ customerLinksRelationshipDTOs + ", customerLinksDTOs=" + customerLinksDTOs
				+ ", listAddress=" + listAddress + ", userDefinedFieldsLinksDTOs="
				+ userDefinedFieldsLinksDTOs + ", guarantors=" + guarantors + ", mezaCardId="
				+ mezaCardId + ", acmMezaCardDTO=" + acmMezaCardDTO
				+ ", disbursementMethodUpdatedToOtherThanMezaCard="
				+ disbursementMethodUpdatedToOtherThanMezaCard + ", disbursementMethodSelected="
				+ disbursementMethodSelected + ", dateInsertion=" + dateInsertion + ", enabled="
				+ enabled + ", action=" + action + ", enableCriticalData=" + enableCriticalData
				+ ", role=" + role + ", updatedBy=" + updatedBy + ", ibCustomerId=" + ibCustomerId
				+ ", udfLinksGroupeFieldsDTOs=" + udfLinksGroupeFieldsDTOs + "]";
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
