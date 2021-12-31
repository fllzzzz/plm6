<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="[{type:1,name:'深化加工'},{type:2,name:'深化加工'}]"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :stripe="false"
        :span-method="objectSpanMethod"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="计划类型" width="180" />
        <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="类型" width="180" >
          <template v-slot="scope">
            <el-tag :type="scope.row.type===2?'danger':''">{{ scope.row.type===1? '时间': '任务' }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('rateProgress')"
          key="rateProgress"
          prop="rateProgress"
          :show-overflow-tooltip="true"
          label="完成进度"
          min-width="320"
        >
          <template v-slot="scope">
            <div v-if="scope.row.type===1">计划用时120天|已用时60天</div>
            <div v-if="scope.row.type===2">{{`总量1200${crud.query.productType===TechnologyTypeAllEnum.STRUCTURE.V?'(t)':'(m)'}|已完成300${crud.query.productType===TechnologyTypeAllEnum.STRUCTURE.V?'(t)':'(m)'}`}}</div>
            <el-progress
              :text-inside="true"
              :stroke-width="26"
              :percentage="50"
              :status="scope.row.type===2?'exception':''"
            />
          </template>
        </el-table-column>
         <el-table-column
          v-if="columns.visible('rate')"
          key="rate"
          prop="rate"
          :show-overflow-tooltip="true"
          label="完成率"
          min-width="280"
        >
          <template v-slot="scope">
            <el-tag :type="scope.row.type===2?'danger':''">50%</el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
    <template v-else>
      <div style="color: red; font-size: 14px">*请先前去合同管理模块添加项目内容</div>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-progress'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['planProgress:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '计划跟踪',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-progress',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    if (rowIndex % 2 === 0) {
      return {
        rowspan: 2,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  // data.data.content = data.data.content.map((v) => {
  //   v.areaTraceDTOList = v.monomerDetail.areaTraceDTOList && v.monomerDetail.areaTraceDTOList.length > 0 ? v.monomerDetail.areaTraceDTOList : []
  //   if (v.areaTraceDTOList.length > 0) {
  //     v.areaTraceDTOList.map(val => {
  //       const deepVal = val.planDetailTraceDTOList.find(k => k.type === areaPlanTypeEnum.ENUM.DEEPEN.V)
  //       const processVal = val.planDetailTraceDTOList.find(k => k.type === areaPlanTypeEnum.ENUM.PROCESS.V)
  //       const installVal = val.planDetailTraceDTOList.find(k => k.type === areaPlanTypeEnum.ENUM.INSTALL.V)
  //       val.deepVal = deepVal
  //       val.processVal = processVal
  //       val.installVal = installVal
  //     })
  //   }
  //   return v
  // })
}
</script>
<style lang="scss" scoped>
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
// .progress-left {
//   font-size: 12px;
//   position: absolute;
//   z-index: 200;
//   top: 50%;
//   transform: translateY(-50%);
//   left: 0;
// }
// .progress-right {
//   font-size: 12px;
//   position: absolute;
//   z-index: 200;
//   top: 50%;
//   transform: translateY(-50%);
//   right: 50px;
// }
</style>
