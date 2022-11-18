<template>
  <common-dialog
    custom-class="requisitions-application-preview"
    title="材料申购汇总"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
    fullscreen
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">申购编号：{{ form.serialNumber }}</el-tag>
      <el-tag type="success" effect="plain" size="medium">申购人：{{ user.name }}</el-tag>
    </template>
    <template #titleRight>
      <el-date-picker
        v-model="form.arrivalTime"
        type="date"
        size="small"
        value-format="x"
        :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
          :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
        placeholder="选择到厂日期"
        style="width: 140px"
      />
      <common-select
        v-model="form.approveProcessId"
        :options="approvalProcessOptions"
        type="other"
        :dataStructure="{ key: 'id', label: 'approveInfoName', value: 'id' }"
        size="small"
        placeholder="选择审批流程"
        :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
        style="width:220px"
      />
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
        >
          <!-- 基础信息 -->
          <material-base-info-columns fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns />
          <!-- 次要信息 -->
          <material-secondary-info-columns :showBatchNo="false" />
        </common-table>
        <div class="table-remark">
          <span>项目</span>
          <span>{{ projectName }}</span>
        </div>
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
import moment from 'moment'

import { regExtra } from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

import commonFooter from './common-footer.vue'
// TODO:处理申购单与项目之间的关联
// TODO: 标签打印提示

const emit = defineEmits(['saveSuccess', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  }
})

const { user } = mapGetters('user')
const { projectMap } = mapGetters('projectMap')
const approvalProcessOptions = ref([])

// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialColumns
])

const { visible: dialogVisible, handleClose } = useVisible({ emit, props })
const { cu, form, FORM } = regExtra() // 表单

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.requisitions-application-preview',
    extraBox: ['.el-dialog__header', '.footer', '.table-remark'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  dialogVisible
)

// 项目名称
const projectName = computed(() => {
  return form.projectId?.map(id => {
    const data = projectMap.value?.[id] || {}
    return `${data.serialNumber} ${data.shortName}`
  })?.join('、')
})

watch(
  () => props.modelValue,
  (val) => {
    approvalProcessOptions.value = []
    if (val && !form.serialNumber) {
      getNO()
    }
    if (val) {
      fetchApprovalProcess()
    }
  },
  { immediate: true }
)

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  return form
}

// 表单提交前校验
FORM.HOOK.beforeSubmit = async () => {
  if (!form.serialNumber) {
    ElMessage.error('获取申购单号失败，无法提交')
    return false
  }
  if (!form.arrivalTime) {
    ElMessage.warning('请选择到厂时间')
    return false
  }
  if (!form.approveProcessId) {
    ElMessage.warning('请选择审批流程')
    return false
  }
}

// 表单提交后：关闭预览窗口
FORM.HOOK.afterSubmit = () => {
  handleClose()
}

// 获取申购单号
async function getNO() {
  const data = await getSerialNumber() || ''
  form.serialNumber = data
}

async function fetchApprovalProcess() {
  const data = await getApprovalProcess() || []
  approvalProcessOptions.value = data
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: [['quantity', 3], 'mete'] })
}
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
    >span:first-child {
      width: 55px;
      line-height: 44px;
      text-align: center;
      border-right: 1px solid #ebeef5;
    }
    >span:last-child {
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
