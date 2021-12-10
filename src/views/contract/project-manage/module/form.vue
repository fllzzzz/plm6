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
      <common-button v-if="stepActive != 0" :disabled="loading" type="warning" size="mini" @click="stepActive--">上一步</common-button>
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
            <project-members v-show="stepActive === 3" ref="projectMembersRef" @getUserIds="userIdsList" />
          </el-form>
        </div>
      </div>
      </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
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

function nextStepValidate(){
  switch(stepActive.value){
    case 0:
      return baseFormRef.value.validateForm();
    case 1:
      return businessFormRef.value.validateForm();
    case 2:
      return customerFormRef.value.validateForm();
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
        stepActive.value ++
      }
    }
  } catch (error) {
    console.log('error', error)
  }
}

CRUD.HOOK.beforeToCU= (crud,form)=>{
  stepActive.value = 0
}

// 保存  手动输入不存在的配置信息
async function saveEnclosureDict(data) {
  if (!data.length) {
    return
  }
  try {
    // await patchAddEnclosureDict(data)
  } catch (error) {
    console.log('围护配置信息保存失败', error)
  }
}

function userIdsList(val) {
  crud.form.userIdList = val
}
// import { patchAddEnclosureDict } from '@/api/contract/common'
// // import { getDetail } from '@/api/contract/ledger'
// import CRUD, { form } from '@crud/crud'
// import baseForm from './base-form'
// import businessForm from './business-form'
// import customerForm from './customer-form'
// import projectMembers from './project-members'

// const defaultForm = {
//   baseRequestVO: {},
//   business: {},
//   customer: {},
//   userIdList: []
// }

// const steps = {
//   0: { K: 'baseForm', L: '基础信息', V: 0 },
//   1: { K: 'businessForm', L: '商务信息', V: 1 },
//   2: { K: 'customerForm', L: '客户信息', V: 2 },
//   3: { K: 'projectMembers', L: '项目成员', V: 3 }
// }
// // Project members
// export default {
//   // eslint-disable-next-line vue/no-unused-components
//   components: { baseForm, businessForm, customerForm, projectMembers },
//   inject: ['crud', 'permission'],
//   mixins: [form(defaultForm)],
//   data() {
//     return {
//       steps,
//       prepared: CRUD.STATUS.PREPARED,
//       stepActive: 0,
//       loading: false
//     }
//   },
//   methods: {
//     userIdsList(val) {
//       this.form.userIdList = val
//     },
//     init() {
//       this.stepActive = 0
//     },
//     async nextStep() {
//       try {
//         const valid = await this.$refs[steps[this.stepActive].K].validate()
//         if (valid) {
//           if (this.stepActive === 3) {
//             // TODO:没有对form重新赋值
//             this.crud.submitCU()
//             const enclosureInfo = this.form.business.enclosureInfo
//             const dictData = []
//             for (const key in enclosureInfo) {
//               const table = enclosureInfo[key] || []
//               for (let i = 0; i < table.length; i++) {
//                 for (const c in table[i].dict) {
//                   if (table[i].dict[c]) {
//                     dictData.push(table[i].dict[c])
//                   }
//                 }
//               }
//             }
//             this.saveEnclosureDict(dictData)
//           } else {
//             this.stepActive++
//           }
//         }
//       } catch (error) {
//         console.log('error', error)
//       }
//     },
//     // 保存  手动输入不存在的配置信息
//     async saveEnclosureDict(data) {
//       if (!data.length) {
//         return
//       }
//       try {
//         await patchAddEnclosureDict(data)
//       } catch (error) {
//         console.log('围护配置信息保存失败', error)
//       }
//     },
//     getDetail() {
//       // 设置业务部门
//     },
//     handleTabClick(val) {

//     },
//     submit() {
//       // 合同立项 - 基本信息保存
//       this.crud.submitCU()
//       // 合同立项 - 商务信息保存
//       // 合同立项 - 客户信息保存
//     },
//     [CRUD.HOOK.beforeToCU]() {
//       this.init()
//     },
//     async [CRUD.HOOK.beforeToEdit]() {
//     //   try {
//     //     const projectDetail = await getDetail(this.form.id)
//     //     projectDetail.files = projectDetail.attachments || []
//     //     projectDetail.region = cleanArray([projectDetail.countryId, projectDetail.provinceId, projectDetail.cityId, projectDetail.regionId])
//     //     Object.assign(this.form, projectDetail)
//     //   } catch (error) {
//     //     console.log(error)
//     //     this.$message({
//     //       message: '设置项目详情失败',
//     //       type: 'error'
//     //     })
//     //   }
//     },
//     // 提交前处理
//     [CRUD.HOOK.beforeSubmit]() {
//       console.log(this.form)
//       // if (this.form.files.length > 0) {
//       //   this.form.attachments = this.form.files.map(f => f.id)
//       // }
//     }
//   }
// }
</script>

<style  rel="stylesheet/scss" lang="scss" scoped>
>>>.el-dialog__body{
    padding: 10px 20px;
}
>>>.el-drawer__header {
    margin-bottom: 10px;
}
.el-drawer-container {
  padding: 0 20px 20px 20px;
  overflow: auto;
}
>>>.el-steps--simple {
  padding: 10px 8%;
}
>>>.el-input-number .el-input__inner {
  text-align: left;
}
>>>.el-form--label-top .el-form-item__label {
    padding: 0 0 1px 0;
}
.form-content {
  height: calc(100vh - 120px);
  overflow: auto;
  .el-form-item {
    margin-right: 0;
  }
}
</style>
