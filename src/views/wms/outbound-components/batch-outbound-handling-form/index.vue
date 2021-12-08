<template>
  <common-dialog
    title="出库办理"
    v-model="dialogVisible"
    width="1600px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-batch-outbound-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="right" inline label-width="70px">
      <div class="form-header">
        <el-form-item label="项目" prop="projectId" label-width="55px">
          <project-cascader v-if="showProjectSelect" v-model="form.projectId" clearable class="input-underline" style="width: 300px" />
          <span v-else v-parse-project="{ project: globalProject }" v-empty-text />
        </el-form-item>
        <el-form-item label="领用人" prop="recipientId">
          <user-dept-cascader
            v-model="form.recipientId"
            :collapse-tags="false"
            clearable
            filterable
            show-all-levels
            placeholder="领用人"
            class="input-underline"
            style="width: 300px"
          />
        </el-form-item>
      </div>
      <common-table
        ref="tableRef"
        :data="form.list"
        :max-height="maxHeight"
        :default-expand-all="false"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row">
              <p>项目：<span v-if="row.project" v-parse-project="{ project: row.project }" v-empty-text /></p>
              <el-input
                v-model="row.remark"
                :autosize="remarkTextSize"
                type="textarea"
                placeholder="备注"
                maxlength="200"
                show-word-limit
                style="max-width: 400px"
              />
            </expand-secondary-info>
          </template>
        </el-expand-table-column>
        <el-table-column label="序号" type="index" align="center" width="50" fixed="left" />
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" show-factory />
        <!-- 单位及其数量 -->
        <material-unit-operate-quantity-columns :basic-class="basicClass" :show-unit="!(basicClass & STEEL_ENUM)" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
        <warehouse-info-columns />
        <el-table-column label="出库数量" width="170px" align="center" fixed="right">
          <template #header>
            <span>出库数量</span>
            <span class="text-clickable" style="margin-left: 10px" @click="setMaxQuantity">全部出库</span>
          </template>
          <template #default="{ row }">
            <span class="flex-rbc">
              <el-input-number
                v-model="row.batchOutboundQuantity"
                :min="0"
                :precision="row.outboundUnitPrecision"
                :max="row.operableQuantity"
                controls-position="right"
              />
              <span style="flex: none; margin-left: 10px">{{ row.outboundUnit }}</span>
            </span>
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import {
  steelPlateBatchOutboundHandling,
  sectionSteelBatchOutboundHandling,
  steelCoilBatchOutboundHandling,
  auxMatBatchOutboundHandling,
  gasBatchOutboundHandling
} from '@/api/wms/outbound/outbound-handling'
import { defineEmits, defineProps, watch, ref, watchEffect, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { STEEL_ENUM } from '@/settings/config'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectCascader from '@comp-base/project-cascader.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { mapGetters } from '@/store/lib'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  projectWarehouseType: {
    type: Number
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  materialList: {
    // 物料出库信息
    type: Array,
    default: () => []
  }
})

// 钢板校验规则
const steelRules = {
  projectId: [{ required: true, message: '请选择出库项目', trigger: 'change' }]
}

// 校验
const rules = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return steelRules
  }
  return {}
})

// 表单ref
const formRef = ref()
// 表格展开key列表
const expandRowKeys = ref([])
// 提交表单
const form = ref({
  list: []
})
// 提交loading
const submitLoading = ref(false)
// 显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: dlgCloseHook })
// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-batch-outbound-handling',
    extraBox: ['.el-dialog__header', '.form-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 350
  },
  dialogVisible
)
// 出库配置
const { outboundCfg } = useWmsConfig()

// 全局项目
const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

// 显示项目选择组件(false:显示项目名称)： 公共库 或者 配置=>项目库可以出库给其他项目
const showProjectSelect = computed(() => {
  return props.projectWarehouseType === projectWarehouseTypeEnum.PUBLIC.V || outboundCfg.value.boolCanOutToOtherProject === true
})

// 备注输入框大小
const remarkTextSize = computed(() => {
  if (props.basicClass === matClsEnum.STEEL_PLATE.V) {
    return { minRows: 3, maxRows: 3 }
  }
  return { minRows: 1, maxRows: 1 }
})

// 监听项目及仓库类型变化
watch(
  [globalProjectId, () => props.projectWarehouseType],
  ([pId, type]) => {
    if (type === projectWarehouseTypeEnum.PUBLIC.V) {
      form.value.projectId = undefined
    } else {
      form.value.projectId = pId
    }
  },
  { immediate: true }
)

// 监听传入的列表
watchEffect(() => {
  form.value.list = props.materialList.filter((v) => v.corOperableQuantity > 0) // 过滤不可操作的列表
})

// 表单初始化
function formInit() {
  form.value = { list: [] }
  formRef.value && formRef.value.resetField()
}

// 关闭回调
function dlgCloseHook() {
  formRef.value && formRef.value.clearValidate()
}

// 设置最大数量
function setMaxQuantity() {
  form.value.list.forEach((v) => {
    v.batchOutboundQuantity = v.corOperableQuantity
  })
}

// 批量出库提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return
    const submitApi = getApi(props.basicClass)
    // 数据格式装换
    const data = {
      projectId: form.value.projectId,
      recipientId: form.value.recipientId,
      list: []
    }
    // 无需进行对列表进行数量是否填写校验，提交时过滤数量为空或为0的数据
    form.value.list.forEach((v) => {
      if (v.batchOutboundQuantity) {
        data.list.push({
          id: v.id,
          quantity: v.quantity,
          outboundUnit: v.outboundUnit,
          outboundUnitType: v.curOutboundUnitType
        })
      }
    })
    if (data.list.length === 0) {
      ElMessage.warning('请填写数据')
      return
    }
    await submitApi(data)
    emit('success')
    handleClose()
    formInit()
  } catch (error) {
    console.log('出库办理', error)
  } finally {
    submitLoading.value = false
  }
}

// 批量出库api
function getApi(basicClass) {
  switch (basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateBatchOutboundHandling
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelBatchOutboundHandling
    case matClsEnum.STEEL_COIL.V:
      return steelCoilBatchOutboundHandling
    case matClsEnum.MATERIAL.V:
      return auxMatBatchOutboundHandling
    case matClsEnum.GAS.V:
      return gasBatchOutboundHandling
    default:
      return null
  }
}
</script>
