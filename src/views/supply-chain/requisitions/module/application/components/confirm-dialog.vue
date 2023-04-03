<template>
  <common-dialog
    custom-class="requisitions-application-preview"
    title="材料申购汇总"
    v-model="dialogVisible"
    :before-close="handleClose"
    fullscreen
  >
    <template #titleAfter>
      <span class="child-mr-6" style="display: flex; align-items: center">
        <el-tag effect="plain" size="medium" v-if="form.projectId">申购项目：{{ projectName }}</el-tag>
        <el-tag effect="plain" size="medium">申购编号：{{ form.serialNumber }}</el-tag>
        <el-tag type="success" effect="plain" size="medium">申购人：{{ user.name }}</el-tag>
        <common-radio-button
          v-if="!props.isManufactured"
          type="enum"
          v-model="requisitionMode"
          :options="requisitionModeEnum.ENUM"
          show-option-all
          clearable
        />
      </span>
    </template>
    <template #titleRight>
      <common-select
        v-if="isOpenApproval"
        v-model="form.approveProcessId"
        :options="approvalProcessOptions"
        type="other"
        :dataStructure="{ key: 'id', label: 'approveInfoName', value: 'id' }"
        size="small"
        placeholder="选择审批流程"
        :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
        style="width: 220px"
      />
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          v-if="!isManufactured"
          :data="showList"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          :cell-class-name="wrongCellMask"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
            <template #default="{ row, $index }">
              <table-cell-tag
                :show="row.requisitionMode === requisitionModeEnum.USE_INVENTORY.V"
                :name="requisitionModeEnum.USE_INVENTORY.L"
                color="#e6a23c"
              />
              <span>{{ $index + 1 }}</span>
            </template>
          </el-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :showIndex="false" fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns />
          <!-- 次要信息 -->
          <material-secondary-info-columns :showBatchNo="false" />
          <el-table-column label="到厂日期" prop="arrivalTime" width="160px" align="center">
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-date-picker
                v-model="row.arrivalTime"
                type="date"
                size="small"
                value-format="x"
                :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
                :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
                :placeholder="row.arrivalTime !== '同上' ? '选择到厂日期' : '同上'"
                @change="handleArrivalTimeChange($event, row, $index)"
                style="width: 100%"
              />
            </template>
          </el-table-column>
        </common-table>
        <!-- 制成品 -->
        <common-table v-else :data="showList" :max-height="maxHeight - 170" :cell-class-name="wrongCellMask" show-summary>
          <el-table-column label="序号" type="index" align="center" width="55" fixed="left" />
          <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
          <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
          <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
          <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
          <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
          <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip />
          <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
          <el-table-column prop="curPurchaseQuantity" label="申购数量" align="center" show-overflow-tooltip />
          <el-table-column prop="curPurchaseWeight" label="申购重量(kg)" align="center" show-overflow-tooltip />
          <el-table-column label="删除数量" align="center" show-overflow-tooltip min-width="100px">
            <template #default="{ row: { sourceRow: row } }">
              <common-input-number
                v-model="row.delPurchaseQuantity"
                :min="1"
                :max="row.curPurchaseQuantity"
                controls-position="right"
                :controls="false"
                :step="5"
                size="mini"
                placeholder="数量"
              />
            </template>
          </el-table-column>
          <el-table-column label="到厂日期" prop="arrivalTime" width="160px" align="center">
            <template #default="{ row: { sourceRow: row }, $index }">
              <el-date-picker
                v-model="row.arrivalTime"
                type="date"
                size="small"
                value-format="x"
                :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
                :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
                :placeholder="row.arrivalTime !== '同上' ? '选择到厂日期' : '同上'"
                @change="handleArrivalTimeChange($event, row, $index)"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="80" align="center">
            <template #default="{ row: { sourceRow: row } }">
              <common-button type="danger" size="mini" icon="el-icon-delete" @click="handleDel(row)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="table-remark">
          <span>备注</span>
          <el-input
            class="remark"
            v-model="form.remark"
            type="textarea"
            :rows="2"
            placeholder="请填写申购备注"
            maxlength="200"
            show-word-limit
          />
        </div>
      </el-form>
    </template>
    <common-footer class="footer" :show-total="false" is-submit />
  </common-dialog>
</template>

<script setup>
import { getSerialNumber } from '@/api/supply-chain/requisitions-manage/requisitions'
import { get as getApprovalProcess } from '@/api/config/approval-config/company-process'
import { defineEmits, defineProps, ref, watch, computed } from 'vue'
import { mapGetters } from '@/store/lib'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialColumns } from '@/utils/columns-format/wms'
import { requisitionModeEnum } from '@enum-ms/wms'
import moment from 'moment'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useApprovalCfg from '@compos/store/use-approval-cfg'
import { ElMessage } from 'element-plus'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

import commonFooter from './common-footer.vue'
// TODO:处理申购单与项目之间的关联
// TODO: 标签打印提示

const emit = defineEmits(['saveSuccess', 'update:modelValue', 'manuf-del'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  isManufactured: {
    type: Boolean,
    default: false
  }
})

const { approvalCfg } = useApprovalCfg()
const { user } = mapGetters('user')
const { projectMap } = mapGetters('projectMap')
const approvalProcessOptions = ref([])

// 表格列数据格式转换
const columnsDataFormat = ref([...materialColumns])

const { visible: dialogVisible, handleClose } = useVisible({ emit, props })
const { cu, form, FORM } = regExtra() // 表单

const tableRules = {
  arrivalTime: [{ required: true, message: '请选择需求完成日期', trigger: 'change' }]
}

const ditto = new Map([['arrivalTime', '同上']])
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

// 是否开启审批
const isOpenApproval = computed(() => approvalCfg.value?.requisition || form.boolInitiateApprove)

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.requisitions-application-preview',
    extraBox: ['.el-dialog__header', '.footer', '.table-remark'],
    wrapperBox: ['.el-dialog__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  dialogVisible
)

// 项目名称
const projectName = computed(() => {
  const data = projectMap.value?.[form.projectId] || {}
  return `${data.serialNumber} ${data.shortName}`
})

watch(
  () => props.modelValue,
  (val) => {
    approvalProcessOptions.value = []
    requisitionMode.value = undefined
    if (val && !form.serialNumber) {
      getNO()
    }
    if (val) {
      fetchApprovalProcess()
    }
    form.list.forEach((v, i) => {
      if (i !== 0) {
        v.arrivalTime = '同上'
      }
    })
  },
  { immediate: true }
)

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  form.list = cleanUpData(form.list)
  if (!props.isManufactured) {
    form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  } else {
    form.list = form.list.map((v) => {
      return {
        artifactEnclosureId: v.id,
        quantity: v.curPurchaseQuantity
      }
    })
  }
  return form
}

// 表单提交前校验
FORM.HOOK.beforeSubmit = async () => {
  if (!form.serialNumber) {
    ElMessage.error('获取申购单号失败，无法提交')
    return false
  }
  // if (!form.arrivalTime) {
  //   ElMessage.warning('请选择到厂时间')
  //   return false
  // }
  const { validResult } = tableValidate(form.list)
  if (!validResult) {
    if (!props.isManufactured && requisitionMode.value) {
      requisitionMode.value = undefined
    }
    return false
  }

  if (!form.approveProcessId && isOpenApproval.value) {
    ElMessage.warning('请选择审批流程')
    return false
  }
}

// 表单提交后：关闭预览窗口
FORM.HOOK.afterSubmit = () => {
  handleClose()
}

function handleArrivalTimeChange(val, row, index) {
  if (index !== 0 && !val) {
    row.arrivalTime = '同上'
  }
}

// 获取申购单号
async function getNO() {
  const data = (await getSerialNumber()) || ''
  form.serialNumber = data
}

async function fetchApprovalProcess() {
  const data = (await getApprovalProcess()) || []
  approvalProcessOptions.value = data
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: [['quantity', 3], 'mete'] })
}

// --------------------------- 申购分类 start ------------------------------
const requisitionMode = ref()

const showList = computed(() => {
  if (props.isManufactured) {
    return form.list.map((v) => {
      v.delPurchaseQuantity = v.curPurchaseQuantity
      return v
    })
  } else {
    return form.list.filter((v) => {
      let flag = true
      if (requisitionMode.value && v.requisitionMode !== requisitionMode.value) {
        flag = false
      }
      return flag
    })
  }
})
// --------------------------- 申购分类 end --------------------------------

// --------------------------- 制成品 start ------------------------------

function handleDel(row) {
  emit('manuf-del', { id: row.id, quantity: row.delPurchaseQuantity }, row)
}

// --------------------------- 制成品 end --------------------------------
</script>

<style lang="scss" scoped>
.requisitions-application-preview {
  position: relative;
  .el-dialog__header .el-tag {
    min-width: 70px;
  }
  .table-remark {
    height: 45px;
    display: flex;
    border: 1px solid #ebeef5;
    border-top-width: 0;
    font-size: 12px;
    color: #606266;
    > span:first-child {
      width: 55px;
      line-height: 44px;
      text-align: center;
      border-right: 1px solid #ebeef5;
    }
    > span:last-child {
      flex: 1;
      height: 40px;
      padding: 6px 10px;
      display: -webkit-box;
      overflow: hidden;
      text-overflow: ellipsis;
      -webkit-line-clamp: 2;
      -webkit-box-orient: vertical;
    }
    ::v-deep(.el-textarea__inner) {
      line-height: 1.26;
    }
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
