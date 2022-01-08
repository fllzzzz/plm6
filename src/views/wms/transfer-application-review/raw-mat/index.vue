<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            审批意见：<span v-empty-text>{{ row.approvalComments }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
          <!-- 是否甲供材料 -->
          <table-cell-tag
            v-if="row.transferCreateType === transferCreateTypeEnum.OUTBOUND.V"
            name="出库"
            :color="TAG_TRANSFER_OUTBOUND_COLOR"
          />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        width="160"
        label="调拨单号"
        align="left"
      >
        <template #default="{ row }">
          <!-- 是否甲供材料 -->
          <table-cell-tag v-if="row.boolHasUnfreeze" name="解冻" :color="TAG_TRANSFER_UNFREEZE_COLOR" :offset="15" />
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        width="100"
        align="center"
      >
        <template #default="{ row }">
          <!-- 目前调拨只支持单种物料的调拨 -->
          <span v-parse-enum="{ e: rawMatClsEnum, v: row.basicClass, bit: true, split: ' | ' }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferType')"
        show-overflow-tooltip
        key="transferType"
        prop="transferType"
        label="调拨类型"
        width="90"
        align="center"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: transferTypeEnum, v: row.transferType }" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('source')" show-overflow-tooltip key="projects" prop="projects" label="来源" min-width="170">
        <template #default="{ row }">
          <template v-for="(item, si) in row.source" :key="si">
            <span
              class="project-ware-text"
              v-if="item.project"
              v-parse-project="{ project: item.project, onlyShortName: true }"
              v-empty-text
            />
            <span class="public-ware-text" v-else>公共库</span>
            <span v-if="item.factory">（{{ item.factory.name }}）</span>
            <span v-if="si !== row.source.length - 1">&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;</span>
          </template>
          <template v-if="row.boolBorrowReturnNotSelf">
            <span class="borrow-direction-icon">▶</span>
            <el-tooltip content="实际借用项目" placement="top">
              <span class="project-ware-text" v-parse-project="{ project: row.borrowProject, onlyShortName: true }" v-empty-text />
            </el-tooltip>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('direction')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="目的"
        min-width="170"
      >
        <template #default="{ row }">
          <template v-if="row.transferType !== transferTypeEnum.RETURN_PARTY_A.V">
            <span
              class="project-ware-text"
              v-if="row.direction.project"
              v-parse-project="{ project: row.direction.project, onlyShortName: true }"
              v-empty-text
            />
            <span class="public-ware-text" v-else>公共库</span>
            <span v-if="row.direction.factory">（{{ row.direction.factory.name }}）</span>
          </template>
          <span v-else v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('founderName')"
        key="founderName"
        :show-overflow-tooltip="true"
        prop="founderName"
        label="申请人"
        align="center"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('editorName')"
        key="editorName"
        :show-overflow-tooltip="true"
        prop="editorName"
        label="编辑人"
        align="center"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('reviewerName')"
        key="reviewerName"
        :show-overflow-tooltip="true"
        prop="reviewerName"
        label="审核人"
        align="center"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="申请日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="编辑日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.userUpdateTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="审核日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.reviewTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewStatus')"
        key="reviewStatus"
        :show-overflow-tooltip="true"
        prop="reviewStatus"
        label="状态"
        align="center"
        width="80"
        fixed="right"
      >
        <template #default="{ row }">
          <template v-if="row.reviewable">
            <common-button type="warning" icon="el-icon-s-check" size="mini" @click="toReview(row)" />
          </template>
          <template v-else>
            <el-tag :type="reviewStatusEnum.V[row.reviewStatus].TAG">{{ reviewStatusEnum.VL[row.reviewStatus] }}</el-tag>
          </template>
        </template>
      </el-table-column>
      <!--详情-->
      <el-table-column label="操作" width="80" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" :show-edit="false" :show-del="false" show-detail />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 审核 -->
    <review v-model:visible="reviewVisible" :data="currentRow" @refresh="crud.refresh" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/wms/transfer/raw-mat-application-review'
import { TAG_TRANSFER_OUTBOUND_COLOR, TAG_TRANSFER_UNFREEZE_COLOR } from '@/settings/config'
import { rawMatClsEnum } from '@enum-ms/classification'
import { transferCreateTypeEnum, transferTypeEnum } from '@/utils/enum/modules/wms'
import { reviewStatusEnum } from '@enum-ms/common'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import mHeader from './module/header'
import udOperation from '@crud/UD.operation.vue'
import pagination from '@crud/Pagination'
import mDetail from './module/detail.vue'
import review from './module/review.vue'
import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_transferApplication_review:get'],
  review: ['wms_transferApplication_review:review']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '入库记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const currentRow = ref({})
const reviewVisible = ref(false)
const expandRowKeys = ref([])
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.reviewable = v.reviewStatus === reviewStatusEnum.UNREVIEWED.V && checkPermission(permission.review)
  })
}

// 打开审核
function toReview(row) {
  currentRow.value = row
  reviewVisible.value = true
}
</script>

<style lang="scss" scoped>
.project-ware-text {
  color: darkgoldenrod;
}
.public-ware-text {
  color: brown;
}

.borrow-direction-icon {
  color: #f00;
  margin-right: 8px;
}

.borrow-direction-icon + span {
  color: #f00;
}
</style>
