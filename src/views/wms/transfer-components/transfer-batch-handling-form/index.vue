<template>
  <common-dialog
    title="调拨办理"
    v-model="dialogVisible"
    width="1600px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-batch-transfer-handling"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <el-form ref="formRef" class="form" :model="form" :rules="rules" size="small" label-position="right" inline label-width="70px">
      <div class="form-header">
        <el-form-item label="类型" prop="transferType" label-width="55px">
          <common-radio
            v-model="form.transferType"
            :options="transferTypeEnum.ENUM"
            :disabled-val="disabledTransferType"
            type="enum"
            size="small"
            @change="handleTypeChange"
          />
        </el-form-item>
        <el-form-item v-if="showProjectSelect" label="项目" prop="projectId">
          <project-cascader v-model="form.projectId" clearable class="input-underline" style="width: 300px" />
        </el-form-item>
        <el-form-item v-if="showFactoryAndWare" label="工厂" prop="factoryId">
          <factory-select v-model="form.factoryId" placeholder="工厂" class="input-underline" style="width: 200px" />
        </el-form-item>
        <el-form-item v-if="showFactoryAndWare" label="仓库" prop="warehouseId">
          <warehouse-select
            v-model="form.warehouseId"
            :factory-id="form.factoryId"
            :basic-class="basicClass"
            placeholder="存储位置"
            class="input-underline"
            style="width: 200px"
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
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" show-factory />
        <!-- 单位及其数量 -->
        <material-unit-operate-quantity-columns :basic-class="basicClass" :show-unit="!(basicClass & STEEL_ENUM)" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" :show-batch-no="false" />
        <warehouse-info-columns />
        <el-table-column label="调拨数量" width="170px" align="center" fixed="right">
          <template #header>
            <span>调拨数量</span>
            <span class="text-clickable" style="margin-left: 10px" @click="setMaxQuantity">全部调拨</span>
          </template>
          <template #default="{ row }">
            <span class="flex-rbc">
              <el-input-number
                v-model="row.batchTransferQuantity"
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
  steelPlateBatchTransferHandling,
  sectionSteelBatchTransferHandling,
  steelCoilBatchTransferHandling,
  auxMatBatchTransferHandling,
  gasBatchTransferHandling
} from '@/api/wms/transfer/transfer-handling'
import { defineEmits, defineProps, ref, watchEffect, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { STEEL_ENUM } from '@/settings/config'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import ProjectCascader from '@comp-base/project-cascader.vue'
import FactorySelect from '@/components-system/base/factory-select.vue'
import WarehouseSelect from '@/components-system/wms/warehouse-select.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { transferTypeEnum } from '@/utils/enum/modules/wms'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  materialList: {
    // 物料调拨信息
    type: Array,
    default: () => []
  }
})

// 校验
const rules = {
  transferType: [{ required: true, message: '请选择调拨类型', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择调拨项目', trigger: 'change' }],
  factoryId: [{ required: true, message: '请选择调拨工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择调拨仓库', trigger: 'change' }]
}

// 表单ref
const formRef = ref()
// 表格展开key列表
const expandRowKeys = ref([])
// 禁用的调拨类型
const disabledTransferType = ref([])
// 过滤后的材料列表
const materialList = ref([])
// 提交表单
const form = ref({
  list: [],
  transferType: transferTypeEnum.PROJECT_WARE.V
})
// 提交loading
const submitLoading = ref(false)
// 显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: clearValidate })
// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-batch-transfer-handling',
    extraBox: ['.el-dialog__header', '.form-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 350
  },
  dialogVisible
)

// 显示项目选择 ：项目仓，且配置为可出库到其他项目的情况下可选择
const showProjectSelect = computed(() => {
  return form.value.transferType === transferTypeEnum.PROJECT_WARE.V
})

// 显示仓库位置选择：归还甲方的情况下无需选择
const showFactoryAndWare = computed(() => {
  return form.value.transferType !== transferTypeEnum.RETURN_PARTY_A.V
})

// 备注输入框大小
const remarkTextSize = computed(() => {
  if (props.basicClass === matClsEnum.STEEL_PLATE.V) {
    return { minRows: 2, maxRows: 2 }
  }
  return { minRows: 1, maxRows: 1 }
})

// 监听传入的列表
watchEffect(() => {
  // 无需在打开dlg时，判断batchTransferQuantity是否大于corOperableQuantity，因为当corOperableQuantity发生变化时，页面及数据会刷新
  let partyANum = 0
  materialList.value = props.materialList.filter((v) => v.corOperableQuantity > 0) // 过滤不可操作的列表
  form.value.list = materialList.value
  materialList.value.forEach(v => {
    if (v.boolPartyA) partyANum++
  })
  if (partyANum) {
    disabledTransferType.value = []
  } else {
    disabledTransferType.value = [transferTypeEnum.RETURN_PARTY_A.V]
  }
})

// 表单初始化
function formInit() {
  form.value = {
    list: [],
    transferType: transferTypeEnum.PROJECT_WARE.V
  }
  formRef.value && formRef.value.resetFields()
}

// 关闭回调
function clearValidate() {
  formRef.value && formRef.value.clearValidate()
}

// 设置最大数量
function setMaxQuantity() {
  form.value.list.forEach((v) => {
    v.batchTransferQuantity = v.corOperableQuantity
  })
}

// 批量调拨提交
async function submit() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return
    const submitApi = getApi(props.basicClass)
    // 数据格式装换
    const data = {
      transferType: form.value.transferType, // 调拨类型
      projectId: form.value.projectId, // 项目id
      factoryId: form.value.factoryId, // 工厂id
      warehouseId: form.value.warehouseId, // 仓库id
      list: []
    }
    // 无需进行对列表进行数量是否填写校验，提交时过滤数量为空或为0的数据
    form.value.list.forEach((v) => {
      if (v.batchTransferQuantity) {
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
    setTimeout(() => {
      formInit()
    }, 0)
  } catch (error) {
    console.log('调拨办理', error)
  } finally {
    submitLoading.value = false
  }
}

// 调拨类型发生变化
function handleTypeChange(type) {
  if (type === transferTypeEnum.RETURN_PARTY_A.V) {
    // 归还甲方，筛选为甲供的类型
    form.value.list = materialList.value.filter(v => v.boolPartyA)
  } else {
    form.value.list = materialList.value
  }
}

// 批量调拨api
function getApi(basicClass) {
  switch (basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateBatchTransferHandling
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelBatchTransferHandling
    case matClsEnum.STEEL_COIL.V:
      return steelCoilBatchTransferHandling
    case matClsEnum.MATERIAL.V:
      return auxMatBatchTransferHandling
    case matClsEnum.GAS.V:
      return gasBatchTransferHandling
    default:
      return null
  }
}
</script>
