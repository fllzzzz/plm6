<template>
  <crudOperation>
    <template v-slot:optRight>
      <div v-show="crud.props.searchToggle" class="opt-right">
        <el-radio-group v-model="query.status" size="small" @change="crud.toQuery">
          <el-radio-button :label="undefined">全部</el-radio-button>
          <el-radio-button v-for="status in statusEnum" :key="status.V" :label="status.V">{{ status.L }}</el-radio-button>
        </el-radio-group>
        <el-date-picker
          v-model="query.year"
          size="small"
          class="date-item"
          type="year"
          placeholder="搜索开始年份"
          value-format="timestamp"
          style="width:150px"
        />
        <rrOperation :crud="crud" />
      </div>
    </template>
  </crudOperation>
</template>

<script>
import { mapGetters } from 'vuex'
import { header } from '@crud/crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
// import { projectStatusEnum as statusEnum } from '@/utils/enum/index'

const defaultQuery = { status: statusEnum.PROCESS.V }
export default {
  components: { rrOperation, crudOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  data() {
    return {
      statusEnum
    }
  },
  computed: {
    ...mapGetters([
      'currentProjectType'
    ])
  },
  watch: {
    currentProjectType: {
      handler(val) {
        this.query.projectType = val
        this.crud.toQuery()
      },
      immediate: true
    }
  },
  created() {
    this.crud.optShow.add = false
    this.crud.optShow.edit = false
    this.crud.optShow.del = false
  }
}
</script>

<style lang="scss" scoped>
.opt-right {
  /deep/ .el-button {
    margin-bottom: 0;
  }
  .date-item {
    margin-bottom: 0;
  }
}
</style>
