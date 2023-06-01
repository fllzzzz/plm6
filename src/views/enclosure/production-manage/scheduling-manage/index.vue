<template>
  <div class="wrap">
    <div class="wrap-left">
      <project-list ref="projectListRef" @project-change="projectChange" />
    </div>
    <div class="wrap-right">
      <div class="app-container">
        <el-tag v-if="!crud.query?.projectId" type="info" effect="plain" size="large"> * 请点击左侧项目列表查看详情 </el-tag>
        <template v-else>
          <!--工具栏-->
          <mHeader class="head-container scheduling-head-container" :project="project" @refresh-project="projectListRef.refresh" />
          <!--表格渲染-->
          <common-table
            ref="tableRef"
            v-loading="crud.loading"
            :data="crud.data"
            :empty-text="crud.emptyText"
            :max-height="maxHeight"
            :data-format="dataFormat"
            row-key="id"
            returnSourceData
            @selection-change="crud.selectionChangeHandler"
          >
            <el-table-column type="selection" width="55" align="center" />
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column key="planName" prop="planName" label="批次" v-if="columns.visible('planName') && crud.query.planIds.length > 1" show-overflow-tooltip align="center" />
            <el-table-column key="name" prop="name" label="名称" v-if="columns.visible('name')" show-overflow-tooltip align="center" />
            <el-table-column
              key="serialNumber"
              prop="serialNumber"
              v-if="columns.visible('serialNumber')"
              show-overflow-tooltip
              label="编号"
              align="center"
            />
            <el-table-column key="plate" prop="plate" v-if="columns.visible('plate') && crud.query.category !== mesEnclosureTypeEnum.FOLDING_PIECE.V" show-overflow-tooltip label="板型" align="center" />
            <el-table-column key="brand" prop="brand" v-if="columns.visible('brand')" show-overflow-tooltip label="品牌" align="center" />
            <el-table-column key="color" prop="color" v-if="columns.visible('color')" show-overflow-tooltip label="颜色" align="center" />
            <el-table-column
              key="length"
              prop="length"
              v-if="columns.visible('length')"
              show-overflow-tooltip
              label="单长(mm)"
              align="right"
            />
            <el-table-column
              key="needSchedulingQuantity"
              prop="needSchedulingQuantity"
              v-if="columns.visible('needSchedulingQuantity')"
              show-overflow-tooltip
              label="待排产数"
              align="center"
            />
            <el-table-column
              key="totalLength"
              prop="totalLength"
              v-if="columns.visible('totalLength')"
              show-overflow-tooltip
              label="总长度(m)"
              align="right"
            />
            <el-table-column
              key="taskQuantity"
              prop="taskQuantity"
              v-if="columns.visible('taskQuantity')"
              show-overflow-tooltip
              label="任务数"
              align="center"
              width="140"
            >
              <template v-slot="{ row }">
                <common-input-number
                  v-model="row.taskQuantity"
                  :max="row.needSchedulingQuantity"
                  :min="0"
                  placeholder="任务数"
                />
              </template>
            </el-table-column>
          </common-table>
          <!--分页组件-->
          <pagination />
        </template>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-manage/scheduling-manage'
import { ref } from 'vue'

import { enclosureSchedulingManagePM as permission } from '@/page-permission/enclosure'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import projectList from './project-list'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const project = ref({})
const projectListRef = ref()

const dataFormat = ref([
  ['totalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]]
])

const { crud, columns } = useCRUD(
  {
    title: '排产管理',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['category', 'planIds']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  extraBox: '.scheduling-head-container',
  paginate: true
})

function projectChange(row = {}) {
  // 防止快速点击时汇总接口报错
  setTimeout(() => {
    project.value = row
    crud.query.projectId = row.id
  }, 300)
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;

  .wrap-left {
    width: 394px;
  }

  .wrap-right {
    flex: 1;
    min-width: 400px;
    .app-container {
      padding-left: 0;
    }
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
