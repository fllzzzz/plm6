<template>
  <div id="pageContainer" class="app-container">
    <el-form ref="formRef" :model="form" inline size="small" label-position="right" label-width="110px">
      <div>
        <div class="form-row">
          <el-form-item label="客户名称" prop="customerUnit">
            <div class="input-underline">
               <template v-if="(detail.customerUnit || originContractInfo.customerUnit) && originContractInfo.customerUnit!==detail.customerUnit">
                  <cell-change-preview :old="originContractInfo.customerUnit" :new="detail.customerUnit" />
                </template>
                <span v-else>{{ detail.customerUnit }}</span>
            </div>
          </el-form-item>
          <el-form-item label="社会统一代码" prop="socialCode">
            <div class="input-underline">
              <template v-if="(detail.socialCode || originContractInfo.socialCode) && originContractInfo.socialCode!==detail.socialCode">
                <cell-change-preview :old="originContractInfo.socialCode" :new="detail.socialCode" />
              </template>
              <span v-else>{{ detail.socialCode }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="联系电话" prop="customerUnitPhone">
            <div class="input-underline">
              <template v-if="(detail.customerUnitPhone || originContractInfo.customerUnitPhone) && originContractInfo.customerUnitPhone!==detail.customerUnitPhone">
                <cell-change-preview :old="originContractInfo.customerUnitPhone" :new="detail.customerUnitPhone" />
              </template>
              <span v-else>{{ detail.customerUnitPhone }}</span>
            </div>
          </el-form-item>
          <el-form-item label="邮箱" prop="customerEmail">
            <div class="input-underline">
              <template v-if="(detail.customerEmail || originContractInfo.customerEmail) && originContractInfo.customerEmail!==detail.customerEmail">
                <cell-change-preview :old="originContractInfo.customerEmail" :new="detail.customerEmail" />
              </template>
              <span v-else class="detail-break">{{ detail.customerEmail }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="省市区" prop="region">
            <div class="input-underline">
              <template v-if="(!judgeSameValue(detail.customerCountryName,originContractInfo.customerCountryName) || !judgeSameValue(detail.customerProvinceName,originContractInfo.customerProvinceName)|| !judgeSameValue(detail.customerCityName,originContractInfo.customerCityName) ||!judgeSameValue(detail.customerRegionName,originContractInfo.customerRegionName))">
                <cell-change-preview :old="(originContractInfo.customerCountryName || '')+(originContractInfo.customerProvinceName || '')+(originContractInfo.customerCityName || '')+(originContractInfo.customerRegionName || '')" :new="(detail.customerCountryName || '')+(detail.customerProvinceName || '')+(detail.customerCityName || '')+(detail.customerRegionName || '')" />
              </template>
              <span v-else>{{detail.customerCountryName}}</span><span>{{detail.customerProvinceName}}</span><span>{{detail.customerCityName}}</span><span>{{detail.customerRegionName}}</span>
            </div>
          </el-form-item>
          <el-form-item label="详细地址" prop="customerAddress">
            <div class="input-underline" style="width:380px;">
              <span class="detail-break">{{ detail.customerAddress }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="银行账户户名" prop="customerBankUserName">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.customerBankUserName,detail.customerBankUserName)">
                <cell-change-preview :old="originContractInfo.customerBankUserName" :new="detail.customerBankUserName" />
              </template>
              <span v-else class="detail-break">{{ detail.customerBankUserName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="银行账号" prop="customerBankCode">
            <div class="input-underline" style="width:260px;">
              <template v-if="!judgeSameValue(originContractInfo.customerBankCode,detail.customerBankCode)">
                <cell-change-preview :old="originContractInfo.customerBankCode" :new="detail.customerBankCode" />
              </template>
              <span v-else>{{ detail.customerBankCode }}</span>
            </div>
          </el-form-item>
          <el-form-item label="开户行" prop="customerBankName">
            <div class="input-underline" style="width:260px;">
              <template v-if="!judgeSameValue(originContractInfo.customerBankName,detail.customerBankName)">
                <cell-change-preview :old="originContractInfo.customerBankName" :new="detail.customerBankName" />
              </template>
              <span v-else>{{ detail.customerBankName }}</span>
            </div>
          </el-form-item>
        </div>
          <div class="form-row">
          <el-form-item label="收货负责人" prop="receivingManager">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.receivingManager,detail.receivingManager)">
                <cell-change-preview :old="originContractInfo.receivingManager" :new="detail.receivingManager" />
              </template>
              <span v-else>{{ detail.receivingManager }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="receivingManagerPhone">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.receivingManagerPhone,detail.receivingManagerPhone)">
                <cell-change-preview :old="originContractInfo.receivingManagerPhone" :new="detail.receivingManagerPhone" />
              </template>
              <span v-else>{{ detail.receivingManagerPhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="结算负责人" prop="settleManager">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.settleManager,detail.settleManager)">
                <cell-change-preview :old="originContractInfo.settleManager" :new="detail.settleManager" />
              </template>
              <span v-else>{{ detail.settleManager }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="settleManagerPhone">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.settleManagerPhone,detail.settleManagerPhone)">
                <cell-change-preview :old="originContractInfo.settleManagerPhone" :new="detail.settleManagerPhone" />
              </template>
              <span v-else>{{ detail.settleManager }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="财务负责人" prop="financeManager">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.financeManager,detail.financeManager)">
                <cell-change-preview :old="originContractInfo.financeManager" :new="detail.financeManager" />
              </template>
              <span v-else>{{ detail.financeManager }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="financeManagerPhone">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.financeManagerPhone,detail.financeManagerPhone)">
                <cell-change-preview :old="originContractInfo.financeManagerPhone" :new="detail.financeManagerPhone" />
              </template>
              <span v-else>{{ detail.financeManagerPhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="审核负责人" prop="auditManager">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.auditManager,detail.auditManager)">
                <cell-change-preview :old="originContractInfo.auditManager" :new="detail.auditManager" />
              </template>
              <span v-else>{{ detail.auditManager }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="auditManagerPhone">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.auditManagerPhone,detail.auditManagerPhone)">
                <cell-change-preview :old="originContractInfo.auditManagerPhone" :new="detail.auditManagerPhone" />
              </template>
              <span v-else>{{ detail.auditManagerPhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="设计负责人" prop="designManager">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.designManager,detail.designManager)">
                <cell-change-preview :old="originContractInfo.designManager" :new="detail.designManager" />
              </template>
              <span v-else>{{ detail.designManager }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="designManagerPhone">
            <div class="input-underline">
              <template v-if="!judgeSameValue(originContractInfo.designManagerPhone,detail.designManagerPhone)">
                <cell-change-preview :old="originContractInfo.designManagerPhone" :new="detail.designManagerPhone" />
              </template>
              <span v-else>{{ props.detail.designManagerPhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div>
          <el-form-item label="备注">
            <template v-if="!judgeSameValue(originContractInfo.customerRemark,detail.customerRemark)">
              <cell-change-preview :old="originContractInfo.customerRemark" :new="detail.customerRemark" />
            </template>
            <span v-else>{{ props.detail.customerRemark }}</span>
          </el-form-item>
        </div>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import cellChangePreview from '@comp-common/cell-change-preview'
import { judgeSameValue } from '@data-type/index'

const formRef = ref()
const form = ref({})

const props = defineProps({
  detail: {
    type: Object,
    default: () => {}
  },
  originContractInfo: {
    type: Object,
    default: () => {}
  }
})

</script>
<style lang="scss" scoped>
.app-container {
  position: relative;
  .operate-btn {
    position: absolute;
    right: 50px;
    top: 20px;
  }
}
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 300px;
  margin-right: 0;
  input {
    border-top: 0;
    border-left: 0;
    border-right: 0;
    border-radius: 0;
  }
}
.form-row {
  width: 100%;
}
span {
  // color:#4482ff #1682e6
  color: #82848a;
}
.detail-break{
  word-break:break-all;
}
</style>
