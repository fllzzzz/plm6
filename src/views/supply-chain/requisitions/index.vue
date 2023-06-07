<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        label="申购日期"
        align="center"
        width="150"
      />
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        prop="applicantName"
        show-overflow-tooltip
        align="center"
        label="申购人"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        show-overflow-tooltip
        align="center"
        label="申购编号"
        width="170"
      />
      <el-table-column
        v-if="columns.visible('materialType')"
        key="materialType"
        prop="materialType"
        show-overflow-tooltip
        align="center"
        label="材料分类"
        width="100"
      >
        <template #default="{ row }">
          <el-tag size="medium" :type="row.materialTypeTag" effect="plain">{{ row.materialType }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        show-overflow-tooltip
        key="project"
        prop="project"
        label="所属项目"
        min-width="170"
      />
      <el-table-column label="采购状态" prop="purchaseCreationState" align="center" width="90">
        <template #default="{ row }">
          <el-tag v-if="row.purchaseCreationState" effect="plain" :type="requisitionStatusEnum.V[row.purchaseCreationState].T">{{
            requisitionStatusEnum.VL[row.purchaseCreationState]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('mete')"
        prop="mete"
        key="mete"
        label="采购进度"
        align="center"
        width="90"
        show-overflow-tooltip
      >
        <template #default="{ row: { sourceRow: row } }">
          <template v-if="checkPermission(permission.trackGet)">
            <span
              v-if="row.materialType === materialPurchaseClsEnum.STEEL.V"
              @click="showTrackList(row)"
              class="tc-primary"
              style="cursor: pointer"
            >
              {{ row.inboundRate }}%
            </span>
            <common-button v-else icon="el-icon-view" size="mini" type="success" @click="showTrackList(row)" />
          </template>
          <template v-else>
            <span>{{ row.materialType === materialPurchaseClsEnum.STEEL.V ? row.inboundRate + '%' : '-' }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('approveInfoName') && isOpenApproval"
        key="approveInfoName"
        prop="approveInfoName"
        show-overflow-tooltip
        align="center"
        label="审批流程"
      />
      <el-table-column
        v-if="columns.visible('reviewStatus') && isOpenApproval"
        key="reviewStatus"
        prop="reviewStatus"
        show-overflow-tooltip
        align="center"
        label="审核状态"
        width="100"
      >
        <template #default="{ row }">
          <el-tag :type="ddReviewStatusEnum.V[row?.sourceRow?.reviewStatus].TAG" size="medium" effect="plain">{{
            row.reviewStatus
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" key="enabled" prop="enabled" label="状态" align="center" width="100">
        <template #default="{ row: { sourceRow: row } }">
          <el-switch
            v-model="row.enabled"
            :disabled="!checkPermission(permission.editStatus) || (isOpenApproval && row.reviewStatus !== ddReviewStatusEnum.PASS.V)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeStatus(row, row.enabled)"
          />
        </template>
      </el-table-column>
      <!--详情与审核-->
      <el-table-column v-permission="[...permission.detail, ...permission.edit, ...permission.del]" align="center" label="操作" width="170">
        <template #default="{ row: { sourceRow: row } }">
          <udOperation
            show-detail
            :data="{ id: row.id }"
            :disabled-edit="
              row.purchaseCreationState !== requisitionStatusEnum.NOT_STARTED.V ||
              Boolean(
                row.reviewStatus & (ddReviewStatusEnum.UNREVIEWED.V | ddReviewStatusEnum.AUDITING.V | ddReviewStatusEnum.PASS.V) &&
                  isOpenApproval
              )
            "
            :disabled-del="
              row.purchaseCreationState !== requisitionStatusEnum.NOT_STARTED.V ||
              Boolean(
                isOpenApproval &&
                  row.reviewStatus & (ddReviewStatusEnum.UNREVIEWED.V | ddReviewStatusEnum.AUDITING.V | ddReviewStatusEnum.PASS.V)
              )
            "
            :del-type="isOpenApproval ? 'warning' : 'danger'"
            :del-icon="isOpenApproval ? 'el-icon-document-delete' : 'el-icon-delete'"
            :permission="permission"
            :delPrompt="`确定${isOpenApproval ? '撤销' : '删除'}本条数据吗？`"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail />
    <track-drawer v-model:visible="trackVisible" :info="itemInfo" />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/supply-chain/requisitions-manage/requisitions'
import { ref, computed } from 'vue'

import { scmRequisitionsPM as permission } from '@/page-permission/supply-chain'
import { ddReviewStatusEnum } from '@enum-ms/dd'
import { requisitionStatusEnum } from '@enum-ms/wms'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import checkPermission from '@/utils/system/check-permission'
import { enabledEnum } from '@enum-ms/common'
import { ElMessageBox } from 'element-plus'
import { convertUnits } from '@/utils/convert/unit'
import { toPrecision } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail.vue'
import trackDrawer from './module/track-drawer'
import udOperation from '@crud/UD.operation.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

const dataFormat = ref([
  ['project', ['parse-project', { onlyShortName: true }]],
  ['reviewStatus', ['parse-enum', ddReviewStatusEnum, { f: 'L' }]],
  ['materialTypeTag', ['parse-enum', materialPurchaseClsEnum, { f: 'T' }], { source: 'materialType' }],
  ['materialType', ['parse-enum', materialPurchaseClsEnum, { f: 'L' }]],
  ['createTime', 'parse-time']
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '材料申购',
    permission: { ...permission },
    invisibleColumns: [],
    // formStore: true,
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const trackVisible = ref(false)
const itemInfo = ref({})

// 是否开启申购审批
const isOpenApproval = computed(() => crud.query.boolInitiateApprove)

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.serialNumber + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, enabled: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('申购单状态', error)
    data.enabled = data.enabled === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

function showTrackList(data) {
  itemInfo.value = data
  trackVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.inboundMete = v.inboundMete || 0
    v.totalMete = v.totalMete || 0
    v.inboundRate = v.inboundMete ? toPrecision((v.inboundMete / v.totalMete) * 100, 2) : 0
    if (v.materialType === materialPurchaseClsEnum.STEEL.V) {
      v.inboundMete = convertUnits(v.inboundMete, 'g', 'kg')
      v.totalMete = convertUnits(v.totalMete, 'g', 'kg')
    }
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
}
</style>
