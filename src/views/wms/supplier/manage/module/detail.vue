<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelDetail"
    :visible="crud.detailVisible"
    :title="crud.detailTitle"
    :show-close="true"
    width="80%"
    fullscreen
  >
    <div class="form">
      <el-form  v-loading="crud.detailLoading" ref="formRef" size="small" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="供应商名称" prop="name">
            <span>{{ detail.name }}</span>
          </el-form-item>
          <el-form-item label="供应商分类" prop="supplierClassificationLabel">
            <div>{{ detail.supplierClassificationLabel }}</div>
          </el-form-item>
          <el-form-item label="主营业务" prop="mainBusiness">
            <div>{{ detail.mainBusiness }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
            <el-form-item label="地区" prop="area">
              {{ `${detail.countryName || ''}${detail.provinceName || ''}${detail.cityName || ''}${detail.regionName || ''}${detail.address || ''}` }}
            </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="社会统一代码" prop="socialCode">
            <div>{{ detail.socialCode }}</div>
          </el-form-item>
          <el-form-item label="成立日期" prop="registrationDate">
            <div>{{ detail.registrationDate }}</div>
          </el-form-item>
          <el-form-item label="营业期限" prop="businessTerm">
            <div>{{ detail.businessTerm }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="法定代表人" prop="legalRepresentative">
            <div>{{ detail.legalRepresentative }}</div>
          </el-form-item>
          <el-form-item label="注册资本" prop="registeredCapital">
            <div>{{ detail.registeredCapital }}</div>
          </el-form-item>
          <el-form-item label="企业类型" prop="enterpriseType">
            <div>{{ detail.enterpriseTypeName }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="开户行名称" prop="bankName">
            <div>{{ detail.bankName }}</div>
          </el-form-item>
          <el-form-item label="银行账户" prop="bankAccount">
            <div>{{ detail.bankAccount }}</div>
          </el-form-item>
          <el-form-item/>
        </div>
        <div class="rule-row">
          <el-form-item label="公司官网" prop="website">
            <div>{{ detail.website }}</div>
          </el-form-item>
          <el-form-item label="公司邮箱" prop="companyEmail">
            <div>{{ detail.companyEmail }}</div>
          </el-form-item>
          <el-form-item label="公司电话" prop="companyPhone">
            <div>{{ detail.companyPhone }}</div>
          </el-form-item>
        </div>
        <div v-for="(item,index) in (detail.contacts || [])" :key="index" class="rule-row">
          <el-form-item label="联系人">
            <div>{{ item.name }}</div>
          </el-form-item>
          <el-form-item label="联系电话">
            <div>{{ item.phone }}</div>
          </el-form-item>
          <el-form-item label="个人邮箱">
            <div>{{ item.email }}</div>
          </el-form-item>
        </div>
        <div class="item-center">
          <upload-list
            :show-download="!!detail.id"
            :uploadable="false"
            :file-classify="fileClassifyEnum.SUPPLIER_ATT.V"
            :download-perm="crud.permission.downloadAttachments"
            :download-fn="downloadAttachment"
            v-model:files="detail.files"
            style="padding: 10px 30px 0"
          />
        </div>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { downloadAttachment } from '@/api/wms/supplier/manage'
import { ref } from 'vue'

import { supplierClassEnum } from '@enum-ms/supplier'
import { fileClassifyEnum } from '@enum-ms/file'
import { getLabelByBit } from '@/utils/enum/base'

import { regDetail } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import uploadList from '@/components/file-upload/uploadList'

const formRef = ref()

const dict = useDict(['enterprise_type'])
const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  try {
    const supplierDetail = { ...detail }
    const list = [supplierDetail.countryId, supplierDetail.provinceId, supplierDetail.cityId, supplierDetail.regionId]
    supplierDetail.area = list.filter(val => {
      return !(!val || val === '')
    })
    supplierDetail.files = supplierDetail.attachments || []
    supplierDetail.supplierClassificationLabel = getLabelByBit(supplierClassEnum, supplierDetail.supplierClassification, '、')
    Object.assign(detail, supplierDetail)
    getEnterpriseTypeName(detail.enterpriseType)
  } catch (error) {
    crud.notify('获取供应商详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 获取企业类型名称
function getEnterpriseTypeName(type) {
  dict.value.enterprise_type.forEach(item => {
    if (item.value === type) detail.enterpriseTypeName = item.label
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.form {
  padding:  0px 60px 35px 35px;

}
.demo-form .rule-row {
  display: flex;
  margin-bottom: 10px;
}
.demo-form .rule-row:last-child {
  margin-bottom: 0px;
}
.form .el-form-item {
  flex: 1;
  width: 33%;
  margin-right: 30px;
}
.form .el-upload__tip {
  padding-left: 15px;
}
</style>
