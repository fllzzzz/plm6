<template>
  <common-drawer append-to-body v-model="visible" :before-close="handleClose" title="出口退税" :wrapper-closable="false" size="100%">
    <template #titleAfter>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right: 6px" v-permission="permission.add">
        添加
      </common-button>
      <el-tag size="medium" effect="plain">
        发货总额：
        <span v-thousand="props.currentRow.deliverInstallAmount"></span>
      </el-tag>
      <el-tag type="success" size="medium" effect="plain">
        已退税发货总额：
        <span v-thousand="totalSendAmount"></span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table v-permission="crud.permission.print" api-key="invoiceRecord" :params="crud.query" size="mini" type="warning" />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :cell-class-name="wrongCellMask"
        :stripe="false"
        return-source-data
        show-summary
        :summary-method="getSummaries"
        :showEmptySymbol="false"
      >
        <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
        <el-table-column key="drawbackDate" prop="drawbackDate" label="退税日期" align="center" width="125">
          <template v-slot="{ row }">
            <el-date-picker
              v-if="row.isModify"
              v-model="row.drawbackDate"
              type="date"
              size="small"
              value-format="x"
              placeholder="请选择退税日期"
              style="width: 100%"
            />
            <span v-else v-parse-time="{ val: row.drawbackDate, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column key="sendAmount" prop="sendAmount" label="发货额" align="center" min-width="110">
          <template v-slot="{ row }">
            <el-input-number
              v-if="row.isModify"
              v-show-thousand
              v-model.number="row.sendAmount"
              :min="0"
              :max="999999999999"
              :step="100"
              :precision="DP.YUAN"
              placeholder="请输入发货额"
              controls-position="right"
            />
            <span v-else v-thousand="row.sendAmount"></span>
          </template>
        </el-table-column>
        <el-table-column key="drawbackAmount" prop="drawbackAmount" label="退税总额" align="center" min-width="110">
          <template v-slot="{ row }">
            <el-input-number
              v-if="row.isModify"
              v-show-thousand
              v-model.number="row.drawbackAmount"
              :min="0"
              :max="999999999999"
              :step="100"
              :precision="DP.YUAN"
              placeholder="请输入退税总额"
              controls-position="right"
            />
            <span v-else v-thousand="row.drawbackAmount"></span>
          </template>
        </el-table-column>
        <el-table-column prop="remark" label="备注" align="center" min-width="100">
          <template v-slot="{ row }">
            <el-input v-if="row.isModify" v-model.trim="row.remark" type="text" placeholder="备注" style="width: 100%" />
            <span v-else>{{ row.remark }}</span>
          </template>
        </el-table-column>
        <el-table-column key="accountantName" prop="accountantName" label="核算人" align="center" width="100px" />
        <el-table-column key="agentName" prop="agentName" label="办理人" align="center" width="100px" />
        <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="100px" />
        <!--编辑与删除-->
        <el-table-column v-if="checkPermission([...permission.edit, ...permission.audit])" label="操作" width="190px" align="center">
          <template v-slot="{ row, $index }">
            <template v-if="!row.isModify">
              <common-button
                icon="el-icon-edit"
                type="primary"
                size="mini"
                @click="modifyRow(row)"
                v-if="row.auditStatus === auditTypeEnum.AUDITING.V && checkPermission(permission.edit)"
              />
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                title="确定删除吗?"
                @confirm="rowDelete(row)"
                v-if="row.auditStatus === auditTypeEnum.AUDITING.V && checkPermission(permission.del)"
              >
                <template #reference>
                  <common-button icon="el-icon-delete" type="danger" size="mini" />
                </template>
              </el-popconfirm>
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                title="确定通过吗?"
                @confirm="passConfirm(row)"
                v-if="row.auditStatus === auditTypeEnum.AUDITING.V && checkPermission(permission.audit)"
              >
                <template #reference>
                  <common-button type="success" size="mini">通过</common-button>
                </template>
              </el-popconfirm>
              <el-tag type="success" size="medium" v-if="row.auditStatus === auditTypeEnum.PASS.V">已复核</el-tag>
            </template>
            <template v-else>
              <el-popconfirm confirm-button-text="确定" cancel-button-text="取消" title="确定取消吗?" @confirm="rowCancel(row, $index)">
                <template #reference>
                  <common-button size="mini">取消</common-button>
                </template>
              </el-popconfirm>
              <common-button type="primary" size="mini" @click="rowSubmit(row)">保存</common-button>
            </template>
          </template>
        </el-table-column>
      </common-table>
      <mForm :currentRow="{ ...props.currentRow, totalSendAmount }" />
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/contract/export-tax-rebate'
import { ref, defineEmits, defineProps, provide, watch } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import { auditTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { contractLedgerPM } from '@/page-permission/contract'

import mForm from './form'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { validate } from '@compos/form/use-table-validate'
import useVisible from '@compos/use-visible'

const permission = contractLedgerPM.exportTaxRebate

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.currentRow,
  (row) => {
    crud.query.projectId = row.id
    crud.toQuery()
  },
  { deep: true }
)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const tableRef = ref()
const originRow = ref({})
const totalSendAmount = ref(0)
provide('totalAmount', totalSendAmount)
const { crud, CRUD } = useCRUD(
  {
    title: '出口退税',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
    requiredQuery: ['projectId']
  },
  tableRef
)

const tableRules = {
  drawbackDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  sendAmount: [{ required: true, message: '请选择开票额', trigger: 'change', type: 'number' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body']
})

async function passConfirm(row) {
  try {
    await editStatus({ id: row.id, auditStatus: auditTypeEnum.PASS.V })
    crud.notify(`审核成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}

function modifyRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
  row.drawbackDate = String(row.drawbackDate)
}

async function rowDelete(row) {
  try {
    await crudApi.del([row.id])
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (e) {
    console.log(`删除失败`, e)
  }
}

function rowCancel(row) {
  row.isModify = false
  if (row.id) {
    row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  } else {
    const index = crud.data.findIndex((v) => v.dataIndex === row.dataIndex)
    crud.data.splice(index, 1)
  }
}

async function rowSubmit(row) {
  try {
    await crudApi.edit(row)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (error) {
    console.log('修改出口退税失败', error)
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['sendAmount', 'drawbackAmount'],
    toThousandFields: ['sendAmount', 'drawbackAmount']
  })
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  totalSendAmount.value = 0
  data.content = data.map((row) => {
    totalSendAmount.value += row.sendAmount || 0
    return row
  })
}
</script>
