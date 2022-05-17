<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
      @expand-change="expandChange"
    >
    
      <el-table-column type="expand">
        <template v-slot='scope'>
        <common-table
              v-loading="scope.row.loadingList"
              :data="scope.row.subList"
              row-key="id"
              style="width: 100%" 
              border
            >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
           <el-table-column
              v-if="columns.visible('thick')"
              header-align="center"
              key="thick"
              prop="thick"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="钢板厚度"
              align="center"
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.thick}} 
                </template> 
             </el-table-column> 
               <el-table-column
              v-if="columns.visible('material')"
              header-align="center"
              key="material"
              prop="material"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="材质"
              align="center"
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.material}} 
                </template> 
             </el-table-column> 
                  <el-table-column
              v-if="columns.visible('sum')"
              header-align="center"
              key="sum"
              prop="sum"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="零件数量"
              align="center"
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.sum}} 
                </template> 
             </el-table-column> 
                <el-table-column
              v-if="columns.visible('reduce')"
              header-align="center"
              key="reduce"
              prop="reduce"
              :show-overflow-tooltip="true"
              style="width: 100%"
              label="零件重量"
              align="center"
              min-width="60"> 
                <template v-slot="scope">
                    {{scope.row.reduce}} 
                </template> 
             </el-table-column> 
                <el-table-column
              v-if="columns.visible('relationType')"
              header-align="center"
              key="relationType"
              prop="relationType"
              :show-overflow-tooltip="true"
              style="width: 50%"
              label="零件属性"
              align="center"
              min-width="60"> 
              <template v-slot="scope">
                <!-- <el-tag type="success">
                  {{PlateTypeEnum.VL[scope.row.relationType]}}
                </el-tag> -->
                <el-tag  v-if='scope.row.relationType&&scope.row.relationType===2' type="success">
                  零件板
                </el-tag> 
                <el-tag  v-else-if='scope.row.relationType&&scope.row.relationType===16' type="danger">
                  翼腹板
                </el-tag>    

              </template>
             </el-table-column> 
             <!-- 任务包零件上传状态 -->
             <el-table-column
        v-if="columns.visible('state')"
        align="center"
        key="state"
        prop="state"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="40"
      >
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" effect="plain" v-if="scope.row.state && scope.row.state === '1'" type="warning">
              已上传
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '0'" type="danger">
              未上传
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '2'" type="success">
              套料结束
            </el-tag>
          </span>
        </template>
      </el-table-column>
            <el-table-column
        v-if="columns.visible('currentName')"
        align="center"
        key="currentName"
        prop="currentName"
        :show-overflow-tooltip="true"
        label="投料人"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.currentName }} {{scope.row.importTime}}</span>
        </template>
      </el-table-column>
        <el-table-column width="250px" :show-overflow-tooltip="true" label="操作" align="center">
          <template v-slot="scope">
            <!-- <common-button type="danger" size="mini" @click="del(scope.row)">删除</common-button>
            <common-button type="success" size="mini" @click.stop="toNesting"> 去套料 </common-button> -->
              <del-btn @query="getPlate" :data="scope.row" />
          </template>
      </el-table-column>
        </common-table>
        </template>

      </el-table-column>
  <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('projectName')"
        header-align="center"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="所属项目"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectNumber }}-{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('sum')"
        header-align="center"
        key="sum"
        prop="sum"
        align="center"
        :show-overflow-tooltip="true"
        label="零件总数"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reduce')"
        header-align="center"
        key="reduce"
        prop="reduce"
        align="center"
        :show-overflow-tooltip="true"
        label="零件重量"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reduce }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNum')"
        header-align="center"
        key="totalNum"
        prop="totalNum"
        align="center"
        :show-overflow-tooltip="true"
        label="已排套数"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projectName')"
        header-align="center"
        key="projectName"
        prop="projectName"
        align="center"
        :show-overflow-tooltip="true"
        label="未排套数"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sum - scope.row.totalNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateNum')"
        header-align="center"
        key="plateNum"
        prop="plateNumplateNum"
        align="center"
        :show-overflow-tooltip="true"
        label="使用钢板数"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateWeight')"
        header-align="center"
        key="plateWeight"
        prop="plateWeight"
        align="center"
        :show-overflow-tooltip="true"
        label="使用钢板重量"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('nestingState')"
        align="center"
        key="nestingState"
        prop="nestingState"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="40"
      >
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" effect="plain"  v-if="scope.row.nestingState === 0" type="danger">
              未套料
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.nestingState && scope.row.nestingState === 1" type="warning">
              部分套料
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.nestingState && scope.row.nestingState === 2" type="success">
              套料结束
            </el-tag>
          </span>
        </template>
      </el-table-column>

      <!-- <el-table-column width="250px" :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <del-btn @query="crud.toQuery()" :data="scope.row" />
        </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi1 from '@/api/cutting/taskPack'
import crudApi from '@/api/cutting/radan-controller'
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import {PlateTypeEnum,NestingEnum} from '@enum-ms/cutting'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { nestingListPM as permission } from '@/page-permission/cutting'
import delBtn from './module/del'

const tableRef = ref()
const dataList = ref([])
const loadingList = ref(false)
const currentRow=ref({})


// crud交由presenter持有
// const permission = {
//   get: ['contractRecord:get'],
//   edit: ['contractRecord:edit'],
//   del: ['contractRecord:del'],
// }

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false,
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '套料工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
    formStore: true,
  },
  tableRef
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, index) => {
    v.subList = []
    v.rowIndex = index
    return v
  })
}

//获取钢板
async function getPlate(){
  try {
    const data = await crudApi1.get({
      nestingState:currentRow.value.nestingState,
      sort: ['createTime.desc'],
      projectId:currentRow.value.projectId
    })
    
    crud.data[currentRow.value.rowIndex].subList = data.content
  } catch (error) {
    console.log('请求接口数据失败')
  }
}

// 请求接口数据
async function expandChange(row, expandedRowsOrExpanded) {
  currentRow.value=row
  console.log(row, expandedRowsOrExpanded)
  loadingList.value = true
  try {
    const data = await crudApi1.get({
      nestingState:row.nestingState,
      sort: ['createTime.desc'],
      projectId:row.projectId
    })
    console.log('data', data)
      
    row.subList = data.content
  } catch (error) {
    console.log('请求接口数据失败')
  }
  loadingList.value = false
}
// 删除操作
function del(row) {
  console.log(row)
}
const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40,
})

// function NestingClick(row) {
//   console.log('row', row.cutTaskId)
// }


</script>
