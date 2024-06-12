//
// Ce fichier a été généré par l'implémentation de référence JavaTM Architecture for XML Binding (JAXB), v2.3.0 
// Voir <a href="https://javaee.github.io/jaxb-v2/">https://javaee.github.io/jaxb-v2/</a> 
// Toute modification apportée à ce fichier sera perdue lors de la recompilation du schéma source. 
// Généré le : 2022.03.17 à 05:09:34 PM GMT+01:00
//


package com.acm.soap.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour LiveResponse complex type.
 * 
 * <p>Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="LiveResponse"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="GetXMLReport" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="GetPDFStream" type="{http://www.w3.org/2001/XMLSchema}base64Binary" minOccurs="0"/&gt;
 *         &lt;element name="OnError" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LiveResponse", propOrder = {
    "getXMLReport",
    "getPDFStream",
    "onError"
})
public class LiveResponse {

    @XmlElement(name = "GetXMLReport")
    protected String getXMLReport;
    @XmlElement(name = "GetPDFStream")
    protected byte[] getPDFStream;
    @XmlElement(name = "OnError")
    protected int onError;

    /**
     * Obtient la valeur de la propriété getXMLReport.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGetXMLReport() {
        return getXMLReport;
    }

    /**
     * Définit la valeur de la propriété getXMLReport.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGetXMLReport(String value) {
        this.getXMLReport = value;
    }

    /**
     * Obtient la valeur de la propriété getPDFStream.
     * 
     * @return
     *     possible object is
     *     byte[]
     */
    public byte[] getGetPDFStream() {
        return getPDFStream;
    }

    /**
     * Définit la valeur de la propriété getPDFStream.
     * 
     * @param value
     *     allowed object is
     *     byte[]
     */
    public void setGetPDFStream(byte[] value) {
        this.getPDFStream = value;
    }

    /**
     * Obtient la valeur de la propriété onError.
     * 
     */
    public int getOnError() {
        return onError;
    }

    /**
     * Définit la valeur de la propriété onError.
     * 
     */
    public void setOnError(int value) {
        this.onError = value;
    }

}
