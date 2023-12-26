<template>
  <el-form
    ref="formRef"
    :model="form"
    :rules="rules"
    size="small"
    label-position="right"
    label-width="100px"
  >
      <el-form-item prop="boolOutbound" label="是否出库" style="margin-top: 10px">
        <el-checkbox v-model="form.boolOutbound" size="large" />
      </el-form-item>
      <el-form-item label="出库目的地" prop="outboundAddress" v-if="form.boolOutbound">
        <common-radio v-model="form.outboundAddress" :options="outboundDestinationTypeEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <el-form-item prop="type" label="钢板类型" style="margin-top: 10px">
        <common-radio
          v-model="form.boolSurplus"
          :options="steelPlateType"
          type="enum"
          size="small"
          disabled
        />
      </el-form-item>
      <el-form-item prop="projectId" label="所属项目" style="margin-top: 10px">
        <common-select
          v-model="form.projectId"
          :options="projectOptions"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          type="other"
          placeholder="所属项目"
          style="width:100%;"
          @change="handleProjectChange"
          :disabled="!showProjectSelect"
        >
          <template #view="{ data }">
            <span :style="`color:${data.id==='common'?'#e6a23c':''}`">{{data.name}}</span>
          </template>
      </common-select>
      </el-form-item>
      <el-form-item prop="monomerId" label="单体" style="margin-top: 10px" v-if="form.projectId!=='common'">
        <common-select
          v-model="form.monomerId"
          :options="projectMap[form.projectId]?.children || []"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          type="other"
          style="width:100%;"
          placeholder="单体"
          @change="handleMonomerChange"
        />
      </el-form-item>
      <el-form-item prop="areaId" label="区域" style="margin-top: 10px" v-if="form.projectId!=='common'">
        <common-select
          v-model="form.areaId"
          :options="monomerMap?.[form.monomerId]?.children || []"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          style="width:100%;"
          type="other"
          placeholder="区域"
        />
      </el-form-item>
      <el-form-item prop="workshopId" label="领用车间" style="margin-top: 10px" v-if="form.boolOutbound">
       <workshop-select v-model="form.workshopId" :factory-id="form.factoryId" placeholder="可选择车间" style="width:100%;" clearable />
      </el-form-item>
      <!-- <el-form-item prop="workshopId1" label="存储车间" style="margin-top: 10px" v-if="!form.boolOutbound">
       <workshop-select v-model="form.workshopId" :factory-id="form.factoryId" placeholder="可选择车间" style="width: 200px" clearable />
      </el-form-item> -->
      <el-form-item prop="warehouseId" label="仓库位置" style="margin-top: 10px">
        <warehouse-select
          v-model="form.warehouseId"
          :factory-id="form.factoryId"
          :basic-class="form.basicClass"
          style="width:100%;"
          placeholder="仓库位置"
        />
      </el-form-item>
      <el-form-item prop="recipientId" label="领用人" style="margin-top: 10px" v-if="form.boolOutbound">
        <user-dept-cascader
          v-model="form.recipientId"
          :collapse-tags="false"
          clearable
          filterable
          show-all-levels
          placeholder="领用人"
          style="width:100%;"
        />
      </el-form-item>
      <el-form-item prop="outboundTime" label="出库日期" style="margin-top: 10px" v-if="form.boolOutbound">
        <el-date-picker
          v-model="form.outboundTime"
          type="date"
          value-format="x"
          placeholder="出库日期"
          style="width:100%;"
        />
      </el-form-item>
  </el-form>
</template>

<script setup>
import { defineProps, defineExpose, computed, ref, watch, nextTick } from 'vue'
import { isNotBlank } from '@/utils/data-type'

import { projectNameFormatter } from '@/utils/project'
import { outboundDestinationTypeEnum } from '@/utils/enum/modules/wms'
import useWmsConfig from '@/composables/store/use-wms-config'

import useProjectTree from '@compos/store/use-project-tree'
import workshopSelect from '@comp-mes/workshop-select'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import warehouseSelect from '@/components-system/wms/warehouse-select.vue'

const steelPlateType = {
  PLATE: { L: '整料', K: 'PLATE', V: false },
  SURPLUS: { L: '余料', K: 'SURPLUS', V: true }
}

const { projectTree, projectMap, monomerMap } = useProjectTree()

// 出库配置
const { outboundCfg } = useWmsConfig()

// 项目选择
const projectOptions = computed(() => {
  if (isNotBlank(projectTree.value)) {
    const arr = projectTree.value.map((p) => {
      return { id: p.id, name: p.id !== 'common' ? projectNameFormatter(p, { showSerialNumber: false }) : '公共库' }
    })
    arr.unshift({ id: 'common', name: '公共库' })
    return arr
  } else {
    return null
  }
})

// 显示项目选择组件(false:显示项目名称)： 公共库 或者 配置=>项目库可以出库给其他项目
const showProjectSelect = computed(() => {
  return outboundCfg.value?.boolCanOutToOtherProject === true
})

const props = defineProps({
  formData: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()

// 表单
const defaultForm = {
  boolOutbound: undefined,
  projectId: undefined,
  monomerId: undefined,
  areaId: undefined,
  workshopId: undefined,
  warehouseId: undefined,
  recipientId: undefined,
  outboundAddress: outboundDestinationTypeEnum.FACTORY.V,
  outboundTime: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))

const rules = {
  // boolOutbound: [{ required: true, message: '是否出库', trigger: 'change' }],
  outboundAddress: [{ required: true, message: '请选择出库目的地', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }],
  // monomerId: [{ required: true, message: '请选择单体', trigger: 'change' }],
  // areaId: [{ required: true, message: '请选择区域', trigger: 'change' }],
  workshopId: [{ required: true, message: '请选择车间', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择仓库位置', trigger: 'change' }],
  outboundTime: [{ required: true, message: '请选择出库日期', trigger: 'change' }]
}

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
  if (formRef.value) {
    formRef.value.resetFields()
  }
  let formVal
  if (data && Object.keys(data).length > 0) {
    formVal = data
  } else {
    formVal = JSON.parse(JSON.stringify(defaultForm))
  }
  form.value = JSON.parse(JSON.stringify(formVal))
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

function resetFields() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
}

function assignForm() {
  const data = JSON.parse(JSON.stringify(form.value))
  Object.assign(props.formData, data)
}

function handleProjectChange() {
  console.log(1)
  form.value.monomerId = undefined
  form.value.areaId = undefined
}

function handleMonomerChange() {
  console.log(2)
  form.value.areaId = undefined
}

// 出库办理，表单提交
async function validateForm() {
  const valid = await formRef.value.validate()
  if (!valid) return false
  const data = JSON.parse(JSON.stringify(form.value))
  Object.assign(props.formData, data)
  return true
}

// 重置表单
function resetForm() {
  // formRef.value.resetFields()
}

// 清空校验
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

defineExpose({
  validateForm,
  resetForm,
  assignForm,
  clearValidate,
  resetFields
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
      line-height: 26px;
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
</style>
