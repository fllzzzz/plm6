<template>
  <common-drawer
    :title="'合同立项'"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :append-to-body="true"
    :close-on-click-modal="false"
    direction="rtl"
    size="80%"
  >
    <template #titleRight>
      <!-- <span class="drawer-title">合同立项</span> -->
      <common-button v-if="stepActive != 0" :disabled="loading" type="warning" size="mini" @click="preStep">上一步</common-button>
      <common-button v-if="stepActive < 3" type="primary" size="mini" :disabled="loading" @click.stop="nextStep">下一步</common-button>
      <common-button v-else :loading="loading" type="success" size="mini" @click.stop="nextStep">提交</common-button>
    </template>
    <template #content>
      <div class="demo-drawer__content">
        <el-steps :active="stepActive" simple>
          <el-step v-for="step in steps" :key="step.V" :title="step.L" icon="el-icon-edit" />
        </el-steps>
        <div class="form-content" style="padding-top:20px">
          <el-form ref="formRef" :model="form">
            <base-form v-show="stepActive === 0" ref="baseFormRef" :form-data="form.baseRequestVO" />
            <business-form v-show="stepActive === 1" ref="businessFormRef" :form-data="form.business" />
            <customer-form v-show="stepActive === 2" ref="customerFormRef" :form-data="form.customer" />
            <project-members v-show="stepActive === 3" ref="projectMembersRef"  :form-data="form.userIdList"/>
          </el-form>
        </div>
      </div>
      </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import baseForm from './base-form'
import businessForm from './business-form'
import customerForm from './customer-form'
import projectMembers from './project-members'

const formRef = ref()
const baseFormRef = ref()
const businessFormRef = ref()
const customerFormRef = ref()
const projectMembersRef = ref()
const defaultForm = {
  baseRequestVO: {},
  business: {},
  customer: {},
  userIdList: []
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const steps = {
  0: { K: 'baseFormRef', L: '基础信息', V: 0 },
  1: { K: 'businessFormRef', L: '商务信息', V: 1 },
  2: { K: 'customerFormRef', L: '客户信息', V: 2 },
  3: { K: 'projectMembersRef', L: '项目成员', V: 3 }
}

const stepActive = ref(0)
const loading = ref(false)

function nextStepValidate() {
  switch (stepActive.value) {
    case 0:
      return baseFormRef.value.validateForm()
    case 1:
      return businessFormRef.value.validateForm()
    case 2:
      return customerFormRef.value.validateForm()
    case 3:
      return projectMembersRef.value.validateForm()
  }
}
async function nextStep() {
  try {
    const valid = await nextStepValidate()
    if (valid) {
      if (stepActive.value === 3) {
        // TODO:没有对form重新赋值
        projectMembersRef.value.getCheckUser()
        crud.form.userIdList = projectMembersRef.value.checkUser || []
        crud.submitCU()
        const enclosureInfo = crud.form.business.enclosureInfo
        const dictData = []
        for (const key in enclosureInfo) {
          const table = enclosureInfo[key] || []
          for (let i = 0; i < table.length; i++) {
            for (const c in table[i].dict) {
              if (table[i].dict[c]) {
                dictData.push(table[i].dict[c])
              }
            }
          }
        }
        // this.saveEnclosureDict(dictData)
      } else {
        stepActive.value++
      }
    }
  } catch (error) {
    console.log('error', error)
  }
}

async function preStep() {
  try {
    const valid = await nextStepValidate()
    if (valid) {
      stepActive.value--
    }
  } catch (error) {
    console.log('error', error)
  }
}

CRUD.HOOK.beforeToCU = (crud, form) => {
  stepActive.value = 0
}

// 保存  手动输入不存在的配置信息
// async function saveEnclosureDict(data) {
//   if (!data.length) {
//     return
//   }
//   try {
//     // await patchAddEnclosureDict(data)
//   } catch (error) {
//     console.log('围护配置信息保存失败', error)
//   }
// }
</script>

<style  rel="stylesheet/scss" lang="scss" scoped>
// >>>.el-dialog__body{
//     padding: 10px 20px;
// }
// >>>.el-drawer__header {
//     margin-bottom: 10px;
// }
// .el-drawer-container {
//   padding: 0 20px 20px 20px;
//   overflow: auto;
// }
// >>>.el-steps--simple {
//   padding: 10px 8%;
// }
// >>>.el-input-number .el-input__inner {
//   text-align: left;
// }
// >>>.el-form--label-top .el-form-item__label {
//     padding: 0 0 1px 0;
// }
.form-content {
  height: calc(100vh - 120px);
  overflow: auto;
  .el-form-item {
    margin-right: 0;
  }
}
</style>
