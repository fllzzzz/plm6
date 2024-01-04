<template>
  <div style="display:flex;">
    <el-card class="box-card">
      <template #header>
        <div class="card-header">
          <span style="font-weight:600;">开平钢板</span>
        </div>
      </template>
      <div style="background:#eee;padding:5px;">
        <div style="display:flex;">
          <div class="flex-div">
            <span class="el-form-item__label span-label">规格</span>
            <span class="span-content">{{form.plateForm.totalObj.specification}}</span>
          </div>
          <div class="flex-div">
            <span class="el-form-item__label span-label span-label-left">开平段数</span>
            <span class="span-content"> {{form.plateForm.totalObj.segmentQuantity}} 段</span>
          </div>
        </div>
        <div style="display:flex;">
          <div class="flex-div">
            <span class="el-form-item__label span-label">厚度</span>
            <span class="span-content"> {{form.plateForm.totalObj.thickness}} mm</span>
          </div>
          <div class="flex-div">
            <span class="el-form-item__label span-label span-label-left">开平板数量</span>
            <span class="span-content"> {{form.plateForm.totalObj.quantity}} 张</span>
          </div>
        </div>
        <div style="display:flex;">
          <div class="flex-div">
            <span class="el-form-item__label span-label">宽度</span>
            <span class="span-content"> {{form.plateForm.totalObj.width}} mm</span>
          </div>
          <div class="flex-div">
            <span class="el-form-item__label span-label span-label-left">开平总重</span>
            <span class="span-content">{{form.plateForm.totalObj.mete}} kg</span>
          </div>
        </div>
      </div>
      <previewForm ref="plateForm" key="plateForm" :form-data="form.plateForm" :material="props.material" />
    </el-card>
    <el-card class="box-card" style="margin-left:10px;">
      <template #header>
        <div class="card-header">
          <span style="font-weight:600;">开平-余料钢板</span>
        </div>
      </template>
      <div v-if="form.surplusForm.totalObj.width>0">
        <div style="background:#eee;padding:5px;">
          <div style="display:flex;">
            <div class="flex-div">
              <span class="el-form-item__label span-label">规格</span>
              <span class="span-content">{{form.surplusForm.totalObj.specification}}</span>
            </div>
            <div class="flex-div">
              <span class="el-form-item__label span-label span-label-left">开平段数</span>
              <span class="span-content">{{form.surplusForm.totalObj.segmentQuantity}}段</span>
            </div>
          </div>
          <div style="display:flex;">
            <div class="flex-div">
              <span class="el-form-item__label span-label">厚度</span>
              <span class="span-content">{{form.surplusForm.totalObj.thickness}} mm</span>
            </div>
            <div class="flex-div">
              <span class="el-form-item__label span-label span-label-left">余料板数量</span>
              <span class="span-content">{{form.surplusForm.totalObj.quantity}} 张</span>
            </div>
          </div>
          <div style="display:flex;">
            <div class="flex-div">
              <span class="el-form-item__label span-label">宽度</span>
              <span class="span-content">{{form.surplusForm.totalObj.width}} mm</span>
            </div>
            <div class="flex-div">
              <span class="el-form-item__label span-label span-label-left">余料总重</span>
              <span class="span-content">{{form.surplusForm.totalObj.mete}} kg</span>
            </div>
          </div>
        </div>
        <previewForm ref="surplusForm" key="surplusForm" :form-data="form.surplusForm" v-if="form.surplusForm.totalObj.width>steelMinLengthConfig?.steelPlateShortestSideMinLength" :material="props.material" />
        <div v-else style="color:red;">根据系统配置，剩余余料规格属于“废料”范围，将纳入废料列表</div>
      </div>
      <div v-else style="width:400px">
        本次无余料
      </div>
    </el-card>
  </div>
</template>

<script setup>
import { defineProps, defineExpose, ref, watch, nextTick } from 'vue'
// 废料定义，退库长度应大于废料
import useSteelMinLengthConfig from '@compos/store/use-steel-minlength-config'

const { steelMinLengthConfig } = useSteelMinLengthConfig()

import previewForm from './preview-form'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料出库信息
    type: Object
  },
  maxHeight: {
    type: Number
  },
  projectWarehouseType: {
    type: [Number, String],
    default: undefined
  },
  formData: {
    type: Object,
    default: () => {}
  }
})

const plateForm = ref()
const surplusForm = ref()
// 表单
const defaultForm = {
  plateForm: {
    totalObj: {}
  },
  surplusForm: {
    totalObj: {}
  }
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))

watch(
  () => props.formData,
  (val) => {
    reset(val)
  },
  { deep: true, immediate: true }
)

/**
 * 重置表单
 */
function reset(data) {
  // 清除表单信息
  if (plateForm.value) {
    plateForm.value.resetFields()
  }
  if (surplusForm.value) {
    surplusForm.value.resetFields()
  }
  let formVal
  if (data && Object.keys(data).length > 0) {
    formVal = data
  } else {
    formVal = JSON.parse(JSON.stringify(defaultForm))
  }
  form.value = JSON.parse(JSON.stringify(formVal))
  nextTick(() => {
    if (surplusForm.value) {
      surplusForm.value && surplusForm.value.clearValidate()
    }
    plateForm.value && plateForm.value.clearValidate()
  })
}

async function validateSubmit() {
  const valid = await plateForm.value.validateForm()
  if (!valid) return false
  let surplusValid = true
  if (surplusForm.value) {
    surplusValid = await surplusForm.value.validateForm()
  }
  if (!surplusValid) return false
  const data = JSON.parse(JSON.stringify(form.value))
  Object.assign(props.formData, data)
  return true
}

function assignForm() {
  plateForm.value && plateForm.value.assignForm()
  if (surplusForm.value) {
    surplusForm.value && surplusForm.value.assignForm()
  }
  const data = JSON.parse(JSON.stringify(form.value))
  Object.assign(props.formData, data)
}

// 重置表单
// function resetForm() {
//   plateForm.value.resetFields()
//   surplusForm.value.resetFields()
// }

// 清空校验
function clearValidate() {
  plateForm.value && plateForm.value.clearValidate()
  if (surplusForm.value) {
    surplusForm.value && surplusForm.value.clearValidate()
  }
}

defineExpose({
  validateSubmit,
  reset,
  assignForm,
  clearValidate
})
</script>

<style lang="scss" scoped>
.set-title {
  font-weight: bold;
  font-size: 16px;
}
.tip {
  display: inline-block;
  color: red;
  margin-left: 15px;
}
.form {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
}
.material-info {
  flex: auto;
}
.form-info {
  margin-left: 20px;
  width: 380px;
  flex: none;
}

.divider {
  display: block;
  height: 1px;
  width: 100%;
  margin: 20px 0;
  border-top: 1px dashed #e9e9e9;
}

.preview-info {
  position: relative;
  width: 100%;
  padding: 0 50px 50px 0;

  .plate-item {
    width: 100%;
    padding: 0 10px;
    box-sizing: border-box;
    background-color: #949090;
    color: #fff;
  }
}

.total-info {
  margin-left: 20px;

  .total-item {
    width: 150px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    border: 1px solid #36ae81;
    border-radius: 5px;

    &:not(:last-child) {
      margin-bottom: 20px;
    }

    .total-label {
      background-color: #36ae81;
      color: #fff;
      height: 30px;
      width: 100%;
      text-align: center;
      line-height: 30px;
    }

    .total-value {
      height: 30px;
      width: 100%;
      text-align: center;
      line-height: 30px;
    }
  }

  .total-item-surplus {
    border-color: #f78230;
    .total-label {
      background-color: #f78230;
    }
  }
}

.other-info {
  // display: flex;
}
.span-label{
  font-weight:600;
  width:60px;
  line-height:30px;
  font-size:14px;
  text-align:center;
  &.span-label-left{
    text-align: left;
    width:100px;
  }
}

.span-content{
  flex:1;
  line-height:30px;
}
.flex-div{
  min-width:220px;
  display:flex;
}
</style>
