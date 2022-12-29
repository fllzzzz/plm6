<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :max-height="maxHeight"
      highlight-current-row
    >
      <!-- 基础信息 -->
      <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
        <template #default="{ row, $index }">
          <!-- 是否甲供材料 -->
          <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classification')"
        prop="classification"
        key="classification"
        label="分类"
        align="center"
        min-width="100"
        fixed="left"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('classifyName')"
        prop="classifyName"
        key="classifyName"
        label="名称"
        align="center"
        show-overflow-tooltip
        min-width="100"
        fixed="left"
      >
        <template #default="{ row }">
          <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
            {{ row.classifyName }}
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        label="规格"
        min-width="160"
        align="left"
      >
        <template #default="{ row }">
          <div class="spec-box">
            <el-popover
              v-if="row.multipleSpec"
              placement="top"
              :title="`${row.classifyName}（${row.specTip}）【${row.sourceFormatSpecArr.length}种】`"
              :width="500"
              trigger="click"
            >
              <template #reference>
                <span class="icon expand-click-8 pointer">
                  <svg-icon icon-class="more" style="flex: none; color: #2c2c2c" />
                </span>
              </template>
              <template #default>
                <div style="margin-top: 20px">
                  <template v-for="(spec, index) in row.sourceFormatSpecArr" :key="spec">
                    <span style="font-weight: bold">{{ spec }}</span>
                    <span v-if="index < row.sourceFormatSpecArr.length - 1">&nbsp;&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;</span>
                  </template>
                </div>
              </template>
            </el-popover>
            <el-tooltip :content="row.specTip" placement="top">
              <span class="spec-info ellipsis-text" :style="{ 'padding-left': row.multipleSpec ? '20px' : 0 }">
                {{ row.formatSpecArr }}
              </span>
            </el-tooltip>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('heatNoAndBatchNo')"
        prop="heatNoAndBatchNo"
        key="heatNoAndBatchNo"
        label="炉批号"
        align="center"
        min-width="120"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('brand')"
        prop="brand"
        key="brand"
        label="品牌"
        align="center"
        min-width="100"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('project')"
        prop="project"
        key="project"
        label="项目"
        align="center"
        min-width="140"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('factory.name')"
        prop="factory.name"
        key="factory.name"
        label="工厂"
        align="center"
        min-width="140"
        show-overflow-tooltip
      />
      <!--详情-->
      <el-table-column
        v-if="checkPermission(permission.detail)"
        label="操作"
        width="80px"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <common-button size="mini" type="info" icon="el-icon-view" @click="showDetail(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <mDetail v-model:visible="detailVisible" :row-detail="rowDetail" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/report/raw-material/inventory-detail'
import { reportRawMaterialInventoryDetailPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import MDetail from './module/detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格列数据格式转换
const dataFormat = ref([
  ['project', ['parse-project', { onlyShortName: true }]],
  ['formatSpecArr', 'split']
])

const rowDetail = ref({})
const detailVisible = ref(false)
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '存货明细帐',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

function showDetail(row) {
  rowDetail.value = Object.assign({ ...crud.query }, row.sourceRow)
  detailVisible.value = true
}
</script>
<style lang="scss" scoped>
.el-table {
  ::v-deep(td .cell) {
    min-height: 28px;
    line-height: 28px;
  }
}
.spec-box {
  position: relative;
  .spec-info {
    display: inline-block;
    width: 100%;
  }
  .icon {
    position: absolute;
    top: 2px;
    left: -2px;
    font-size: 18px;
    .svg-icon {
      color: #409eff !important;
    }
  }
}
</style>
