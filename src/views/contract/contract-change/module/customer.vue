<template>
  <div id="pageContainer" class="app-container">
    <el-form ref="formRef" :model="form" inline size="small" label-position="right" label-width="110px">
      <div>
        <div class="form-row">
          <el-form-item label="客户名称" prop="customerUnit">
            <div class="input-underline">
              <span>{{ detail.customerUnit }}</span>
            </div>
          </el-form-item>
          <el-form-item label="社会统一代码" prop="socialCode">
            <div class="input-underline">
              <span>{{ detail.socialCode }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="联系电话" prop="customerUnitPhone">
            <div class="input-underline">
              <span>{{ detail.customerUnitPhone }}</span>
            </div>
          </el-form-item>
          <el-form-item label="邮箱" prop="customerEmail">
            <div class="input-underline">
              <span class="detail-break">{{ detail.customerEmail }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="省市区" prop="region">
            <div class="input-underline">
              <span>{{detail.customerCountryName}}</span><span>{{detail.customerProvinceName}}</span><span>{{detail.customerCityName}}</span><span>{{detail.customerRegionName}}</span>
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
              <span class="detail-break">{{ detail.customerBankUserName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="银行账号" prop="customerBankCode">
            <div class="input-underline" style="width:260px;">
              <span>{{ detail.customerBankCode }}</span>
            </div>
          </el-form-item>
          <el-form-item label="开户行" prop="customerBankName">
            <div class="input-underline" style="width:260px;">
              <span>{{ detail.customerBankName }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="负责人1" prop="customerManagerOne">
            <div class="input-underline">
              <span>{{ detail.customerManagerOne }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="customerManagerOnePhone">
            <div class="input-underline" style="width:260px;">
              <span>{{ detail.customerManagerOnePhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="负责人2" prop="customerManagerTwo">
            <div class="input-underline">
              <span>{{ detail.customerManagerTwo }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="customerManagerTwoPhone">
            <div class="input-underline" style="width:260px;">
              <span>{{ props.detail.customerManagerTwoPhone }}</span>
            </div>
          </el-form-item>
        </div>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'

const formRef = ref()
const form = ref({})

const props = defineProps({
  detail: {
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
