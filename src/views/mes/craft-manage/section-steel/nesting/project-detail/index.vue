<template>
  <div class="app-container">
    <div v-show="!props.projectData.id">
      <div class="my-code">*点击左侧项目行查看详情</div>
    </div>
    <div v-show="props.projectData.id">
      <common-table
        ref="tableRef"
        :data="projectList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        row-key="id"
        style="width: 100%"
        :span-method="spanMethod"
      >
        <el-table-column align="center" key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体">
          <template v-slot="scope">
            <span>{{ scope.row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="date" prop="date" :show-overflow-tooltip="true" label="完成日期">
          <template v-slot="scope">
            <span>{{ parseTime(scope.row.date, '{y}-{m}-{d}') }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="部件数量">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="部件重量">
          <template v-slot="scope">
            <span>{{ scope.row.totalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="nestingQuantity " prop="nestingQuantity " :show-overflow-tooltip="true" label="可套数量（件）">
          <template v-slot="scope">
            <table-cell-tag
              :name="projectNestingStatusEnum.VL[scope.row.nestingStatusEnum]"
              :color="projectNestingStatusEnum.V[scope.row.nestingStatusEnum].COLOR"
              :offset="15"
            />
            <span>{{ scope.row.nestingQuantity  }}</span>
          </template>
        </el-table-column>
        <el-table-column v-permission="permission.detail" align="center" :show-overflow-tooltip="true" label="操作">
          <template v-slot="scope">
            <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
          </template>
        </el-table-column>
      </common-table>
      <list-detail v-model:visible="innerVisible" :assemble-list="assembleList" />
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { getProjectNesting } from '@/api/mes/craft-manage/section-steel/nesting'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { projectNestingStatusEnum } from '@enum-ms/mes'
import { mesExtrusionNestingPM as permission } from '@/page-permission/mes'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import listDetail from '../list-detail/index.vue'

const props = defineProps({
  projectData: {
    type: Object,
    default: () => {}
  }
})
const { maxHeight } = useMaxHeight({
  paginate: true
})
const innerVisible = ref(false)
const assembleList = ref({})
const projectList = ref([])

watch(
  () => props.projectData.id,
  (val) => {
    if (val) {
      projectDetail()
    }
  }
)

async function projectDetail() {
  if (!props.projectData.id) {
    return
  }
  const _list = []
  try {
    const { content } = await getProjectNesting({
      projectId: props.projectData.id
    })
    console.log(content, 'content')
    content?.map((v) => {
      if (v.assembleBatchAreaDOList?.length) {
        v.assembleBatchAreaDOList.map((k, index) => {
          _list.push({
            monomerName: v.name,
            areaName: k.name,
            ...k,
            rowspan: index === 0 ? v.assembleBatchAreaDOList?.length : 0
          })
        })
      }
    })
    projectList.value = _list
    console.log(projectList.value)
  } catch (e) {
    console.log('获取项目套料进度详情失败', e)
  }
}

function views(row) {
  console.log(row, 'row')
  innerVisible.value = true
  assembleList.value = row
}

// 合并行
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 0) {
    return {
      rowspan: row.rowspan || 0,
      colspan: 1
    }
  }
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
